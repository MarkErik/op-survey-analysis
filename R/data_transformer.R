# R/data_transformer.R
# Data transformation functions for the CPSC Experience Survey Explorer

# =============================================================================
# Main Data Transformation Pipeline
# =============================================================================

#' Transform survey data from raw to processed format
#'
#' Performs comprehensive data cleaning and transformation including:
#' - Likert scale conversion to numeric
#' - Discord multi-select parsing
#' - Participant ID generation
#' - Section parsing
#' - Free-text cleaning
#'
#' @param raw_data Tibble with raw survey data from CSV
#' @return Tibble with processed data
#' @export
transform_survey_data <- function(raw_data) {
  tryCatch({
    # Start with a copy of the data
    processed_data <- raw_data %>%
      dplyr::mutate(
        # Add participant_id
        participant_id = dplyr::if_else(
          is.na(COL_TIMESTAMP) | COL_TIMESTAMP == "",
          NA_character_,
          {
            # Count occurrences of this timestamp/section combination
            seq_count <- dplyr::n() - dplyr::row_number() + 1
            create_participant_id(COL_TIMESTAMP, COL_SECTION, seq_count)
          }
        ),

        # Parse section into course and time
        section_course = dplyr::if_else(
          is.na(COL_SECTION) | COL_SECTION == "",
          NA_character_,
          {
            section_match <- regmatches(COL_SECTION, regexpr("[0-9]+", COL_SECTION, perl = TRUE))
            if (length(section_match) == 0) NA_character_ else section_match[1]
          }
        ),

        section_time = dplyr::if_else(
          is.na(COL_SECTION) | COL_SECTION == "",
          NA_character_,
          {
            time_match <- regmatches(COL_SECTION, regexpr("[a-zA-Z]+", COL_SECTION, perl = TRUE))
            if (length(time_match) == 0) NA_character_ else time_match[1]
          }
        )
      )

    # Convert Likert columns to numeric
    processed_data <- normalize_likert_columns(processed_data)

    # Parse Discord multi-select columns
    processed_data <- parse_multi_select_columns(processed_data)

    # Clean free-text columns
    processed_data <- clean_free_text_columns(processed_data)

    # Convert categorical columns to factors
    processed_data <- convert_categorical_columns(processed_data)

    message("Data transformation completed successfully")
    return(processed_data)

  }, error = function(e) {
    message(sprintf("[ERROR] Data transformation failed: %s", conditionMessage(e)))
    showNotification(
      sprintf("Data transformation error: %s", conditionMessage(e)),
      type = "error",
      duration = ERROR_TOAST_DURATION
    )
    return(raw_data)
  })
}

# =============================================================================
# Likert Scale Normalization
# =============================================================================

#' Normalize Likert scale columns to numeric values
#'
#' Converts Likert-style responses (e.g., "1 - Strongly Disagree") to numeric values (1-5).
#'
#' @param data Tibble with raw survey data
#' @return Tibble with normalized Likert columns
#' @export
normalize_likert_columns <- function(data) {
  # Likert columns to normalize
  likert_columns <- c(
    COL_CONTENT_RELEVANT,
    COL_EXCITED_CONTENT,
    COL_SATISFIED_FEEDBACK,
    COL_APPLY_LEARNING,
    COL_EASY_ASK_HELP,
    COL_MEETING_GOALS,
    COL_PRE_WRITTEN_CODE,
    COL_STUDYING_MIDTERMS,
    COL_TOPHAT_QUIZZES,
    COL_PRESENTATION_SLIDES,
    COL_HANDOUTS_NOTES,
    COL_CODING_OWN,
    COL_LIVE_CODING,
    COL_LABS,
    COL_ASK_QUESTIONS,
    COL_ASSIGNMENTS,
    COL_COMFORTABLE_SPEAKING,
    COL_PART_OF_CLASS,
    COL_FRIENDS_IMPORTANT,
    COL_UNIVERSITY_COMMUNITY,
    COL_EASY_MEET_PEOPLE
  )

  # Normalize each Likert column
  for (col in likert_columns) {
    data[[col]] <- dplyr::if_else(
      is.na(data[[col]]) | data[[col]] == "",
      NA_integer_,
      extract_likert_value(data[[col]])
    )
  }

  return(data)
}

# =============================================================================
# Discord Multi-Select Parsing
# =============================================================================

#' Parse Discord multi-select columns
#'
#' Converts semicolon-separated Discord responses into binary columns for each option.
#'
#' @param data Tibble with raw survey data
#' @return Tibble with parsed Discord columns
#' @export
parse_multi_select_columns <- function(data) {
  # Discord column to parse
  discord_column <- COL_DISCORD

  # Parse Discord responses
  data[[discord_column]] <- dplyr::if_else(
    is.na(data[[discord_column]]) | data[[discord_column]] == "",
    setNames(rep(FALSE, length(DISCORD_OPTIONS)), DISCORD_OPTIONS),
    parse_discord_field(data[[discord_column]])
  )

  return(data)
}

# =============================================================================
# Free-Text Cleaning
# =============================================================================

#' Clean free-text response columns
#'
#' Removes extra whitespace, normalizes line breaks, and handles special characters.
#'
#' @param data Tibble with raw survey data
#' @return Tibble with cleaned free-text columns
#' @export
clean_free_text_columns <- function(data) {
  # Free-text columns to clean
  free_text_columns <- c(
    COL_EXPECTATIONS,
    COL_PREFERENCE_REASON,
    COL_NOT_PREFERRED_REASON,
    COL_IMPROVEMENTS,
    COL_FAVORITE_PART,
    COL_LEAST_ENJOYABLE,
    COL_CHALLENGE_MEETING_PEOPLE,
    COL_INCLUSIVITY,
    COL_STUDENT_INTERACTION,
    COL_PROFESSOR_INTERACTION,
    COL_GENERAL_COMMENTS
  )

  # Clean each free-text column
  for (col in free_text_columns) {
    data[[col]] <- dplyr::if_else(
      is.na(data[[col]]) | data[[col]] == "",
      NA_character_,
      clean_text_field(data[[col]])
    )
  }

  return(data)
}

# =============================================================================
# Categorical Column Conversion
# =============================================================================

#' Convert categorical columns to factors
#'
#' Converts categorical columns (experience, learning preference) to factor variables.
#'
#' @param data Tibble with processed data
#' @return Tibble with categorical columns as factors
#' @export
convert_categorical_columns <- function(data) {
  # Convert experience to factor
  if (COL_EXPERIENCE %in% names(data)) {
    data[[COL_EXPERIENCE]] <- as.factor(data[[COL_EXPERIENCE]])
  }

  # Convert learning preference to factor
  if (COL_LEARNING_PREF %in% names(data)) {
    data[[COL_LEARNING_PREF]] <- as.factor(data[[COL_LEARNING_PREF]])
  }

  return(data)
}

# =============================================================================
# Derived Column Functions
# =============================================================================

#' Add derived columns to data
#'
#' Creates additional derived columns from existing data.
#'
#' @param data Tibble with processed data
#' @return Tibble with derived columns
#' @export
add_derived_columns <- function(data) {
  # Add derived columns if they don't exist
  if (!"section_course" %in% names(data)) {
    data$section_course <- dplyr::if_else(
      is.na(COL_SECTION) | COL_SECTION == "",
      NA_character_,
      {
        section_match <- regmatches(COL_SECTION, regexpr("[0-9]+", COL_SECTION, perl = TRUE))
        if (length(section_match) == 0) NA_character_ else section_match[1]
      }
    )
  }

  if (!"section_time" %in% names(data)) {
    data$section_time <- dplyr::if_else(
      is.na(COL_SECTION) | COL_SECTION == "",
      NA_character_,
      {
        time_match <- regmatches(COL_SECTION, regexpr("[a-zA-Z]+", COL_SECTION, perl = TRUE))
        if (length(time_match) == 0) NA_character_ else time_match[1]
      }
    )
  }

  if (!"participant_id" %in% names(data)) {
    data$participant_id <- dplyr::if_else(
      is.na(COL_TIMESTAMP) | COL_TIMESTAMP == "",
      NA_character_,
      {
        seq_count <- dplyr::n() - dplyr::row_number() + 1
        create_participant_id(COL_TIMESTAMP, COL_SECTION, seq_count)
      }
    )
  }

  return(data)
}

# =============================================================================
# Data Quality Checks
# =============================================================================

#' Check data completeness
#'
#' Returns a summary of missing values by column.
#'
#' @param data Tibble to check
#' @return Tibble with missing value counts
#' @export
check_data_completeness <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }

  missing_counts <- sapply(data, function(x) sum(is.na(x) | x == ""))
  missing_counts <- missing_counts[missing_counts > 0]

  tibble::tibble(
    column = names(missing_counts),
    missing_count = missing_counts
  )
}

#' Check for duplicate rows
#'
#' Identifies duplicate rows based on key columns.
#'
#' @param data Tibble to check
#' @return Tibble with duplicate rows
#' @export
check_duplicates <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }

  # Check duplicates based on timestamp and section
  duplicates <- data %>%
    dplyr::group_by(COL_TIMESTAMP, COL_SECTION) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  return(duplicates)
}

# =============================================================================
# Data Export
# =============================================================================

#' Export processed data to CSV
#'
#' Saves the processed data to a CSV file.
#'
#' @param data Tibble to export
#' @param file_path Character path for output file
#' @return Logical TRUE if successful
#' @export
export_processed_data <- function(data, file_path = "survey_data/processed_survey_data.csv") {
  tryCatch({
    # Create directory if it doesn't exist
    dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

    # Write CSV
    readr::write_csv(data, file_path)

    message(sprintf("Data exported to %s", file_path))
    return(TRUE)

  }, error = function(e) {
    message(sprintf("[ERROR] Export failed: %s", conditionMessage(e)))
    return(FALSE)
  })
}
