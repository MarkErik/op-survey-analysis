# =============================================================================
# GLOBAL.R - Global Configuration and Utility Functions
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LIBRARY LOADING
# -----------------------------------------------------------------------------

load_required_packages <- function() {
  required_packages <- c("shiny", "DT", "tidyverse", "ggiraph", "readr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", ")))
  }
  invisible(TRUE)
}

load_required_packages()

# -----------------------------------------------------------------------------
# 2. APPLICATION CONFIGURATION
# -----------------------------------------------------------------------------

DATA_FILE_PATH <- "survey_data/CPSC Experience Survey.csv"

# Column indices (1-based)
COL_TIMESTAMP <- 1L; COL_SECTION <- 2L; COL_PROGRAMMING_EXP <- 3L
COL_EXPECTATIONS <- 4L; COL_LEARNING_PREF <- 5L; COL_LEARNING_PREF_REASON <- 6L
COL_NOT_PREF_REASON <- 7L
COL_COURSE_SATISFACTION_START <- 8L; COL_COURSE_SATISFACTION_END <- 13L
COL_LEARNING_METHODS_START <- 14L; COL_LEARNING_METHODS_END <- 23L
COL_COURSE_IMPROVEMENTS <- 24L; COL_FAVORITE_PART <- 25L; COL_LEAST_ENJOYABLE <- 26L
COL_COMMUNITY_START <- 27L; COL_COMMUNITY_END <- 31L
COL_SOCIAL_CHALLENGES <- 32L; COL_DISCORD <- 33L; COL_INCLUSIVITY <- 34L
COL_STUDENT_INTERACTION <- 35L; COL_PROF_INTERACTION <- 36L; COL_OTHER_COMMENTS <- 37L

DISCORD_OPTIONS <- c(
  "joined" = "I have joined the class Discord",
  "active" = "I am active in the class Discord",
  "useful" = "It is really useful for me for learning"
)

# -----------------------------------------------------------------------------
# 3. DATA LOADING FUNCTIONS
# -----------------------------------------------------------------------------

load_survey_data <- function(file_path = DATA_FILE_PATH) {
  tryCatch({
    if (!file.exists(file_path)) {
      warning(paste("Data file not found:", file_path))
      return(NULL)
    }
    data <- readr::read_csv(file_path, col_types = readr::cols(.default = readr::col_character()),
                            show_col_types = FALSE, na = c("", "NA", "N/A"))
    message(paste("Loaded", nrow(data), "survey responses"))
    return(data)
  }, error = function(e) {
    warning(paste("Error loading survey data:", e$message))
    return(NULL)
  })
}

validate_survey_data <- function(data, expected_cols = 37L) {
  if (is.null(data)) return(FALSE)
  if (ncol(data) != expected_cols) {
    warning(paste("Expected", expected_cols, "columns, got", ncol(data)))
    return(FALSE)
  }
  return(TRUE)
}

# -----------------------------------------------------------------------------
# 4. DATA CLEANING FUNCTIONS
# -----------------------------------------------------------------------------

clean_likert_scale <- function(response) {
  if (is.na(response) || response == "") return(NA_real_)
  numeric_value <- stringr::str_extract(as.character(response), "[1-5]")
  if (is.na(numeric_value)) return(NA_real_)
  return(as.numeric(numeric_value))
}

parse_section_identifier <- function(section_string) {
  if (is.na(section_string) || section_string == "") {
    return(list(course_number = NA_character_, time_slot = NA_character_))
  }
  parts <- stringr::str_split(trimws(section_string), "\\s*-\\s*", simplify = TRUE)
  if (length(parts) < 2) {
    return(list(course_number = trimws(parts[1]), time_slot = NA_character_))
  }
  return(list(course_number = trimws(parts[1]), time_slot = trimws(parts[2])))
}

parse_discord_field <- function(discord_string) {
  result <- lapply(names(DISCORD_OPTIONS), function(opt) 0L)
  names(result) <- names(DISCORD_OPTIONS)
  if (is.na(discord_string) || discord_string == "") return(result)
  selections <- stringr::str_split(discord_string, ";\\s*", simplify = FALSE)[[1]]
  for (opt_name in names(DISCORD_OPTIONS)) {
    if (any(stringr::str_detect(selections, stringr::regex(DISCORD_OPTIONS[[opt_name]], ignore_case = TRUE)))) {
      result[[opt_name]] <- 1L
    }
  }
  return(result)
}

sanitize_free_text <- function(text, max_length = 5000L) {
  if (is.na(text) || text == "") return(NA_character_)
  text <- trimws(as.character(text))
  text <- stringr::str_replace_all(text, "\\s+", " ")
  text <- stringr::str_replace_all(text, "[[:cntrl:]]", "")
  if (nchar(text) > max_length) {
    text <- paste0(stringr::str_sub(text, 1, max_length - 3), "...")
  }
  return(text)
}

process_survey_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  data$response_id <- seq_len(nrow(data))
  
  # Parse section identifiers
  section_data <- purrr::map(data[[COL_SECTION]], parse_section_identifier)
  data$course_number <- purrr::map_chr(section_data, "course_number")
  data$time_slot <- purrr::map_chr(section_data, "time_slot")
  
  # Clean Likert scales - Course Satisfaction (cols 8-13)
  for (i in COL_COURSE_SATISFACTION_START:COL_COURSE_SATISFACTION_END) {
    data[[paste0("satisfaction_", i)]] <- purrr::map_dbl(data[[i]], clean_likert_scale)
  }
  
  # Clean Likert scales - Learning Methods (cols 14-23)
  for (i in COL_LEARNING_METHODS_START:COL_LEARNING_METHODS_END) {
    data[[paste0("method_", i)]] <- purrr::map_dbl(data[[i]], clean_likert_scale)
  }
  
  # Clean Likert scales - Community (cols 27-31)
  for (i in COL_COMMUNITY_START:COL_COMMUNITY_END) {
    data[[paste0("community_", i)]] <- purrr::map_dbl(data[[i]], clean_likert_scale)
  }
  
  # Parse Discord multi-select
  discord_data <- purrr::map(data[[COL_DISCORD]], parse_discord_field)
  data$discord_joined <- purrr::map_int(discord_data, "joined")
  data$discord_active <- purrr::map_int(discord_data, "active")
  data$discord_useful <- purrr::map_int(discord_data, "useful")
  
  # Sanitize free text fields
  data$expectations <- purrr::map_chr(data[[COL_EXPECTATIONS]], sanitize_free_text)
  data$favorite_part <- purrr::map_chr(data[[COL_FAVORITE_PART]], sanitize_free_text)
  data$least_enjoyable <- purrr::map_chr(data[[COL_LEAST_ENJOYABLE]], sanitize_free_text)
  data$social_challenges <- purrr::map_chr(data[[COL_SOCIAL_CHALLENGES]], sanitize_free_text)
  data$inclusivity <- purrr::map_chr(data[[COL_INCLUSIVITY]], sanitize_free_text)
  data$student_interaction <- purrr::map_chr(data[[COL_STUDENT_INTERACTION]], sanitize_free_text)
  data$prof_interaction <- purrr::map_chr(data[[COL_PROF_INTERACTION]], sanitize_free_text)
  data$other_comments <- purrr::map_chr(data[[COL_OTHER_COMMENTS]], sanitize_free_text)
  
  message("Survey data processing complete")
  return(data)
}

# -----------------------------------------------------------------------------
# 5. SHARED UTILITY FUNCTIONS
# -----------------------------------------------------------------------------

calculate_descriptive_stats <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(list(n = 0L, mean = NA_real_, median = NA_real_, sd = NA_real_,
                min = NA_real_, max = NA_real_, q1 = NA_real_, q3 = NA_real_))
  }
  return(list(
    n = length(x), mean = mean(x, na.rm = TRUE), median = stats::median(x),
    sd = stats::sd(x), min = min(x), max = max(x),
    q1 = stats::quantile(x, 0.25), q3 = stats::quantile(x, 0.75)
  ))
}

get_frequency_table <- function(x, sort_by_freq = TRUE) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(data.frame(value = character(), count = integer(), percent = numeric()))
  }
  counts <- table(x)
  if (sort_by_freq) counts <- sort(counts, decreasing = TRUE)
  return(data.frame(
    value = names(counts), count = as.integer(counts),
    percent = round(as.numeric(counts) / sum(counts) * 100, 1), row.names = NULL
  ))
}

create_section_label <- function(course_number, time_slot) {
  if (is.na(course_number) && is.na(time_slot)) return("Unknown")
  if (is.na(time_slot)) return(course_number)
  return(paste(course_number, time_slot, sep = " - "))
}

coalesce_value <- function(value, default = "N/A") {
  if (is.na(value) || value == "" || is.null(value)) return(default)
  return(value)
}

format_number <- function(x, digits = 0L) {
  if (is.na(x)) return("N/A")
  return(format(round(x, digits), big.mark = ",", scientific = FALSE))
}

# -----------------------------------------------------------------------------
# 6. DATA LOADING AT STARTUP
# -----------------------------------------------------------------------------

survey_data <- load_survey_data(DATA_FILE_PATH)

if (!is.null(survey_data) && validate_survey_data(survey_data)) {
  survey_data <- process_survey_data(survey_data)
  message("Global data loaded successfully")
} else {
  warning("Failed to load survey data - application may have limited functionality")
}