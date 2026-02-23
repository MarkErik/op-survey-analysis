# R/data_loader.R
# Data loading functions for the CPSC Experience Survey Explorer

# =============================================================================
# Load Survey Data
# =============================================================================

#' Load survey data from CSV file
#'
#' Reads the survey data CSV file with proper error handling and validation.
#'
#' @param file_path Character path to the CSV file (default: DATA_FILE_PATH)
#' @return Tibble with raw survey data
#' @export
load_survey_data <- function(file_path = DATA_FILE_PATH) {
  tryCatch({
    # Check if file exists
    if (!file.exists(file_path)) {
      stop("Data file not found: ", file_path,
           "\nPlease ensure the CSV file is in the correct location.")
    }

    # Read CSV file with proper encoding
    data <- readr::read_csv(
      file_path,
      show_col_types = FALSE,
      locale = readr::locale(encoding = DATA_ENCODING)
    )

    # Validate data structure
    if (!validate_data(data)) {
      stop("Invalid data structure. Required columns are missing.")
    }

    # Check if data is empty
    if (nrow(data) == 0) {
      stop("Data file is empty.")
    }

    message(sprintf("Successfully loaded %d rows from %s", nrow(data), file_path))
    return(data)

  }, error = function(e) {
    # Log error
    message(sprintf("[ERROR] Data loading failed: %s", conditionMessage(e)))

    # Show user-friendly error
    showNotification(
      sprintf("Unable to load survey data: %s", conditionMessage(e)),
      type = "error",
      duration = ERROR_TOAST_DURATION
    )

    # Return empty tibble
    return(tibble::tibble())
  })
}

#' Validate survey data structure
#'
#' Checks that required columns exist in the loaded data.
#'
#' @param data Tibble to validate
#' @return Logical TRUE if valid, FALSE otherwise
#' @export
validate_data <- function(data) {
  # Required columns based on data_details.md
  required_cols <- c(
    COL_TIMESTAMP,
    COL_SECTION,
    COL_EXPERIENCE,
    COL_LEARNING_PREF
  )

  # Check for missing columns
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }

  # Check for empty data
  if (nrow(data) == 0) {
    warning("Data file is empty")
    return(FALSE)
  }

  return(TRUE)
}

#' Validate section filter
#'
#' Checks if a section is valid based on the known sections.
#'
#' @param section Character section identifier to validate
#' @return Logical TRUE if valid, FALSE otherwise
#' @export
validate_section <- function(section) {
  if (is.null(section)) {
    return(TRUE)
  }

  return(section %in% SECTIONS)
}

#' Get available sections from data
#'
#' Extracts unique section values from the loaded data.
#'
#' @param data Tibble with survey data
#' @return Character vector of unique sections
#' @export
get_available_sections <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }

  # Extract section values, removing NULL/empty
  sections <- data[[COL_SECTION]]
  sections <- sections[!is.na(sections) & sections != ""]

  # Return unique sections sorted
  unique(sections)[order(unique(sections))]
}

#' Get unique values for a column
#'
#' Extracts unique values from a column, handling NULL/NA values.
#'
#' @param data Tibble
#' @param column_name Character column name
#' @return Character vector of unique values
#' @export
get_unique_values <- function(data, column_name) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }

  values <- data[[column_name]]
  values <- values[!is.na(values) & values != ""]

  return(unique(values))
}

# =============================================================================
# Data Loading Utilities
# =============================================================================

#' Check if data file exists
#'
#' @param file_path Character path to check
#' @return Logical TRUE if file exists
#' @export
check_data_file_exists <- function(file_path = DATA_FILE_PATH) {
  file.exists(file_path)
}

#' Get data file size
#'
#' @param file_path Character path to file
#' @return File size in bytes
#' @export
get_data_file_size <- function(file_path = DATA_FILE_PATH) {
  if (!file.exists(file_path)) {
    return(0)
  }
  file.info(file_path)$size
}

#' Get data file modification time
#'
#' @param file_path Character path to file
#' @return Character timestamp of last modification
#' @export
get_data_file_mod_time <- function(file_path = DATA_FILE_PATH) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  file.info(file_path)$mtime
}

#' Reload data from file
#'
#' Forces a reload of the data file, useful for testing or when file changes.
#'
#' @param file_path Character path to CSV file
#' @return Tibble with reloaded data
#' @export
reload_data <- function(file_path = DATA_FILE_PATH) {
  message("Reloading data from ", file_path)
  load_survey_data(file_path)
}
