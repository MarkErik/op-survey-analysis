# Data Processing Utilities
# Helper functions for data cleaning, transformation, and preprocessing
#
# @author Course Instructor
# @version 2.0.0

#' Clean text responses
#'
#' Removes extra whitespace and normalizes text
#'
#' @param text Character vector to clean
#' @return Cleaned character vector
#' @export
clean_text <- function(text) {
  if (is.na(text) || is.null(text)) {
    return(NA_character_)
  }

  text <- stringr::str_trim(text)
  text <- stringr::str_squish(text)
  text <- stringr::str_replace_all(text, "\\s+", " ")

  # Return NA for empty strings
  if (text == "") {
    return(NA_character_)
  }

  return(text)
}

#' Parse Likert response to numeric
#'
#' Extracts numeric value from Likert scale responses
#'
#' @param x Character vector of Likert responses
#' @return Numeric vector of parsed values
#' @export
parse_likert <- function(x) {
  if (is.na(x) || is.null(x) || x == "") {
    return(NA_integer_)
  }

  # Extract first numeric value from string
  numeric_val <- as.integer(stringr::str_extract(x, "^\\d+"))

  if (is.na(numeric_val)) {
    return(NA_integer_)
  }

  # Return NA if outside valid range, otherwise return the value
  if (!dplyr::between(numeric_val, 1, 5)) {
    return(NA_integer_)
  }
  return(numeric_val)
}

#' Process multiple Likert columns
#'
#' Converts multiple Likert response columns to numeric format
#'
#' @param data Survey data frame
#' @param likert_cols Vector of column names containing Likert responses
#' @return Data frame with processed Likert columns
#' @export
process_likert_columns <- function(data, likert_cols) {
  for (col in likert_cols) {
    if (col %in% colnames(data)) {
      data[[col]] <- sapply(data[[col]], parse_likert)
    }
  }
  return(data)
}

#' Extract section information
#'
#' Parses section identifier into course number and time slot
#'
#' @param section Section string (e.g., "231 - 1pm")
#' @return List with course_number and time_slot
#' @export
extract_section_info <- function(section) {
  if (is.na(section) || is.null(section) || section == "") {
    return(list(course_number = NA, time_slot = NA))
  }

  parts <- stringr::str_split_fixed(section, " - ", n = 2)

  return(list(
    course_number = trimws(parts[1]),
    time_slot = trimws(parts[2])
  ))
}

#' Process Discord multi-select responses
#'
#' Splits semicolon-separated Discord responses into separate columns
#'
#' @param data Survey data frame
#' @return Data frame with added Discord indicator columns
#' @export
process_discord <- function(data) {
  # Find Discord column by pattern (handles R's name conversion)
  discord_col <- grep("Discord.*select all that apply", colnames(data), value = TRUE, ignore.case = TRUE)[1]

  if (is.na(discord_col)) {
    return(data)
  }

  # Initialize indicator columns
  data$discord_joined <- FALSE
  data$discord_active <- FALSE
  data$discord_useful <- FALSE

  # Parse responses
  for (i in seq_len(nrow(data))) {
    response <- data[[discord_col]][i]
    if (!is.na(response) && response != "") {
      responses <- stringr::str_split(response, ";")[[1]]
      data$discord_joined[i] <- any(stringr::str_detect(responses, "joined"))
      data$discord_active[i] <- any(stringr::str_detect(responses, "active"))
      data$discord_useful[i] <- any(stringr::str_detect(responses, "useful"))
    }
  }

  return(data)
}

#' Filter data by section
#'
#' Filters survey data to a specific section
#'
#' @param data Survey data frame
#' @param section Section identifier or NULL for all
#' @return Filtered data frame
#' @export
filter_section <- function(data, section = NULL) {
  if (is.null(section) || is.na(section) || section == "") {
    return(data)
  }

  # Check for common section column names
  section_col <- c("What section are you in?", "section")
  section_col <- section_col[section_col %in% colnames(data)][1]
  
  if (!is.na(section_col)) {
    return(dplyr::filter(data, .data[[section_col]] == section))
  }

  return(data)
}

#' Get Likert column names
#'
#' Identifies columns containing Likert scale responses
#'
#' @param data Survey data frame
#' @return Vector of column names
#' @export
get_likert_cols <- function(data) {
  likert_patterns <- c(
    "How much do you agree",
    "How much do the following elements contribute to your learning?",
    "How much do you agree with the following statements?"
  )

  cols <- colnames(data)
  likert_cols <- character(0)

  for (pattern in likert_patterns) {
    matches <- stringr::str_detect(cols, stringr::regex(pattern, ignore_case = TRUE))
    likert_cols <- c(likert_cols, cols[matches])
  }

  return(unique(likert_cols))
}

#' Calculate response rates
#'
#' Computes response rates for each question
#'
#' @param data Survey data frame
#' @param cols Vector of column names
#' @return Data frame with response rates
#' @export
calculate_response_rates <- function(data, cols) {
  n_total <- nrow(data)

  rates <- lapply(cols, function(col) {
    n_valid <- sum(!is.na(data[[col]]) & data[[col]] != "", na.rm = TRUE)
    data.frame(
      question = col,
      n_responses = n_valid,
      response_rate = n_valid / n_total * 100
    )
  })

  do.call(rbind, rates)
}

#' Reshape data to long format
#'
#' Converts wide survey data to long format for analysis
#'
#' @param data Survey data frame
#' @param id_cols Vector of ID column names to keep
#' @param value_cols Vector of value column names to melt
#' @return Long format data frame
#' @export
reshape_to_long <- function(data, id_cols, value_cols) {
  existing_id_cols <- id_cols[id_cols %in% colnames(data)]
  existing_value_cols <- value_cols[value_cols %in% colnames(data)]

  if (length(existing_value_cols) == 0) {
    return(data.frame())
  }

  reshape2::melt(
    data,
    id.vars = existing_id_cols,
    measure.vars = existing_value_cols,
    variable.name = "question",
    value.name = "response"
  )
}

#' Calculate category means
#'
#' Computes mean scores for each question in a category
#'
#' @param data Survey data frame
#' @param questions Vector of question column names
#' @return Data frame with question means
#' @export
calculate_category_means <- function(data, questions) {
  existing_cols <- questions[questions %in% colnames(data)]

  if (length(existing_cols) == 0) {
    return(data.frame())
  }

  means <- sapply(existing_cols, function(col) {
    mean(data[[col]], na.rm = TRUE)
  })

  data.frame(
    question = names(means),
    mean = means,
    stringsAsFactors = FALSE
  )
}

#' Identify missing data patterns
#'
#' Analyzes patterns in missing data
#'
#' @param data Survey data frame
#' @return Data frame with missing data summary
#' @export
analyze_missing_patterns <- function(data) {
  missing_summary <- data.frame(
    column = colnames(data),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    pct_missing = sapply(data, function(x) sum(is.na(x)) / length(x) * 100),
    stringsAsFactors = FALSE
  )

  missing_summary <- missing_summary %>%
    dplyr::arrange(dplyr::desc(pct_missing))

  return(missing_summary)
}

#' Normalize Likert scores
#'
#' Normalizes Likert scores to 0-1 scale
#'
#' @param x Numeric vector of Likert scores
#' @return Normalized vector
#' @export
normalize_likert <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(numeric(0))
  }

  (x - 1) / 4
}

#' Create response frequency table
#'
#' Generates frequency table for Likert responses
#'
#' @param data Survey data frame
#' @param question_col Column name
#' @return Data frame with frequencies
#' @export
create_frequency_table <- function(data, question_col) {
  if (!question_col %in% colnames(data)) {
    return(data.frame())
  }

  values <- data[[question_col]]
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(data.frame())
  }

  freq_table <- data.frame(
    value = 1:5,
    count = sapply(1:5, function(x) sum(values == x)),
    stringsAsFactors = FALSE
  )

  freq_table$percentage <- freq_table$count / sum(freq_table$count) * 100

  return(freq_table)
}

#' Aggregate by section
#'
#' Aggregates survey data by section
#'
#' @param data Survey data frame
#' @param value_cols Columns to aggregate
#' @return Aggregated data frame
#' @export
aggregate_by_section <- function(data, value_cols) {
  # Check for common section column names
  section_col <- c("What section are you in?", "section")
  section_col <- section_col[section_col %in% colnames(data)][1]

  if (is.na(section_col)) {
    return(data.frame())
  }

  existing_value_cols <- value_cols[value_cols %in% colnames(data)]

  if (length(existing_value_cols) == 0) {
    return(data.frame())
  }

  # Use dplyr::across for modern dplyr compatibility
  data %>%
    dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
    dplyr::group_by(.data[[section_col]]) %>%
    dplyr::summarize(
      dplyr::across(existing_value_cols, list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        n = ~sum(!is.na(.))
      ), .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
}

#' Detect outliers
#'
#' Identifies outliers using IQR method
#'
#' @param x Numeric vector
#' @param threshold IQR multiplier (default 1.5)
#' @return Logical vector indicating outliers
#' @export
detect_outliers <- function(x, threshold = 1.5) {
  x <- x[!is.na(x)]

  if (length(x) < 4) {
    return(rep(FALSE, length(x)))
  }

  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1

  lower <- q1 - threshold * iqr
  upper <- q3 + threshold * iqr

  x < lower | x > upper
}

#' Impute missing values
#'
#' Imputes missing values with category mean
#'
#' @param data Survey data frame
#' @param question_col Column to impute
#' @param group_col Column to group by (optional)
#' @return Data frame with imputed values
#' @export
impute_missing <- function(data, question_col, group_col = NULL) {
  if (!question_col %in% colnames(data)) {
    return(data)
  }

  if (is.null(group_col)) {
    # Impute with overall mean
    mean_val <- mean(data[[question_col]], na.rm = TRUE)
    data[[question_col]][is.na(data[[question_col]])] <- mean_val
  } else {
    # Impute with group mean
    if (group_col %in% colnames(data)) {
      for (grp in unique(data[[group_col]])) {
        grp_mean <- mean(data[data[[group_col]] == grp, ][[question_col]], na.rm = TRUE)
        idx <- is.na(data[[question_col]]) & data[[group_col]] == grp
        data[[question_col]][idx] <- grp_mean
      }
    }
  }

  return(data)
}
