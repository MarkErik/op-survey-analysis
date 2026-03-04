load_survey_data <- function(file_path = DATA_FILE_PATH) {
  tryCatch({
    if (!file.exists(file_path)) {
      stop("Data file not found: ", file_path,
           "\nPlease ensure the CSV file is in the correct location.")
    }

    data <- readr::read_csv(
      file_path,
      show_col_types = FALSE,
      locale = readr::locale(encoding = DATA_ENCODING)
    )

    if (!validate_data(data)) {
      stop("Invalid data structure. Required columns are missing.")
    }

    if (nrow(data) == 0) {
      stop("Data file is empty.")
    }

    message(sprintf("Successfully loaded %d rows from %s", nrow(data), file_path))
    return(data)

  }, error = function(e) {
    message(sprintf("[ERROR] Data loading failed: %s", conditionMessage(e)))

    showNotification(
      sprintf("Unable to load survey data: %s", conditionMessage(e)),
      type = "error",
      duration = ERROR_TOAST_DURATION
    )

    return(tibble::tibble())
  })
}

validate_data <- function(data) {
  required_cols <- c(
    COL_TIMESTAMP,
    COL_SECTION,
    COL_EXPERIENCE,
    COL_LEARNING_PREF
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }

  if (nrow(data) == 0) {
    warning("Data file is empty")
    return(FALSE)
  }

  return(TRUE)
}

validate_section <- function(section) {
  if (is.null(section)) {
    return(TRUE)
  }

  return(section %in% SECTIONS)
}

get_available_sections <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }

  sections <- data[[COL_SECTION]]
  sections <- sections[!is.na(sections) & sections != ""]

  return(unique(sections)[order(unique(sections))])
}

get_unique_values <- function(data, column_name) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }

  values <- data[[column_name]]
  values <- values[!is.na(values) & values != ""]

  return(unique(values))
}

check_data_file_exists <- function(file_path = DATA_FILE_PATH) {
  file.exists(file_path)
}

get_data_file_size <- function(file_path = DATA_FILE_PATH) {
  if (!file.exists(file_path)) {
    return(0)
  }
  file.info(file_path)$size
}

get_data_file_mod_time <- function(file_path = DATA_FILE_PATH) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  file.info(file_path)$mtime
}

reload_data <- function(file_path = DATA_FILE_PATH) {
  message("Reloading data from ", file_path)
  load_survey_data(file_path)
}
