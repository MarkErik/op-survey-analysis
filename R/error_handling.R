getErrorMessage <- function(error_type = "unknown") {
  messages <- list(
    "data_not_found" = "The data file could not be found. Please check the file path and ensure the file exists.",
    "data_empty" = "The data file is empty. Please check the CSV file contents.",
    "data_corrupted" = "The data file appears to be corrupted. Please check the file format.",
    "data_loading" = "There was an error loading the data. Please try again.",
    "data_processing" = "There was an error processing the data. Please try again.",
    "filter_invalid" = "The selected filter value is invalid. Please select a valid option.",
    "filter_missing" = "Required filter values are missing. Please provide all required information.",
    "input_invalid" = "The input provided is invalid. Please check your selection.",
    "unknown" = "An unexpected error occurred. Please try again or contact support."
  )
  
  return(messages[[error_type]] %||% messages[["unknown"]])
}

handle_data_error <- function(data_file_path, error_type = "unknown") {
  log_error(error_type, data_file_path)
  
  show_error_notification(error_type)
  
  return(FALSE)
}

show_error_notification <- function(error_type = "unknown", duration = ERROR_TOAST_DURATION) {
  message <- getErrorMessage(error_type)
  
  showNotification(
    message,
    type = "error",
    duration = duration
  )
}

log_error <- function(error_type = "unknown", context = NULL, level = "ERROR") {
  if (LOG_LEVEL == "OFF") {
    return(invisible(NULL))
  }
  
  if (level != "ERROR" && LOG_LEVEL != "DEBUG") {
    return(invisible(NULL))
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] [%s] %s", timestamp, level, error_type)
  
  if (!is.null(context)) {
    log_entry <- paste0(log_entry, " - ", context)
  }
  
  if (LOG_TO_CONSOLE) {
    message(log_entry)
  }
  
  if (LOG_TO_FILE) {
    if (!dir.exists(dirname(LOG_FILE_PATH))) {
      dir.create(dirname(LOG_FILE_PATH), recursive = TRUE)
    }
    write(log_entry, file = LOG_FILE_PATH, append = TRUE)
  }
  
  return(invisible(NULL))
}

validate_inputs <- function(input_value, validation_type = "required", options = NULL) {
  if (validation_type == "required") {
    if (is.null(input_value) || input_value == "") {
      show_error_notification("input_invalid")
      return(FALSE)
    }
  }
  
  if (validation_type == "options" && !is.null(options)) {
    if (!(input_value %in% options)) {
      show_error_notification("filter_invalid")
      return(FALSE)
    }
  }
  
  if (validation_type == "numeric") {
    if (!is.numeric(input_value) || is.na(input_value)) {
      show_error_notification("input_invalid")
      return(FALSE)
    }
  }
  
  if (validation_type == "range") {
    if (is.null(input_value) || is.na(input_value)) {
      show_error_notification("input_invalid")
      return(FALSE)
    }
  }
  
  return(TRUE)
}

safe_reactive <- function(reactive_expr, error_handler = NULL) {
  return(reactive({
    tryCatch({
      result <- reactive_expr()
      
      if (is.null(result)) {
        if (!is.null(error_handler)) {
          error_handler("result_null")
        } else {
          show_error_notification("data_loading")
        }
        return(NULL)
      }
      
      return(result)
      
    }, error = function(e) {
      error_msg <- conditionMessage(e)
      log_error("reactive_error", error_msg)
      
      if (!is.null(error_handler)) {
        error_handler("reactive_error", error_msg)
      } else {
        show_error_notification("data_loading")
      }
      
      return(NULL)
    })
  }))
}

safe_function <- function(func, error_handler = NULL) {
  return(function(...) {
    tryCatch({
      result <- func(...)
      
      if (is.null(result)) {
        if (!is.null(error_handler)) {
          error_handler("result_null")
        } else {
          show_error_notification("data_loading")
        }
        return(NULL)
      }
      
      return(result)
      
    }, error = function(e) {
      error_msg <- conditionMessage(e)
      log_error("function_error", error_msg)
      
      if (!is.null(error_handler)) {
        error_handler("function_error", error_msg)
      } else {
        show_error_notification("data_loading")
      }
      
      return(NULL)
    })
  })
}

show_warning_notification <- function(message, duration = WARNING_TOAST_DURATION) {
  showNotification(
    message,
    type = "warning",
    duration = duration
  )
}

show_success_notification <- function(message, duration = ERROR_TOAST_DURATION) {
  showNotification(
    message,
    type = "message",
    duration = duration
  )
}

check_data_file <- function(data_file_path) {
  if (!file.exists(data_file_path)) {
    return(list(
      status = "error",
      message = "Data file not found",
      error_type = "data_not_found"
    ))
  }
  
  if (!file.info(data_file_path)$isfile) {
    return(list(
      status = "error",
      message = "Data file is not a valid file",
      error_type = "data_corrupted"
    ))
  }
  
  file_size <- file.info(data_file_path)$size
  if (file_size == 0) {
    return(list(
      status = "error",
      message = "Data file is empty",
      error_type = "data_empty"
    ))
  }
  
  if (!file.access(data_file_path, 4)) {
    return(list(
      status = "error",
      message = "Data file is not readable",
      error_type = "data_loading"
    ))
  }
  
  return(list(
    status = "ok",
    message = "Data file is valid",
    file_size = file_size
  ))
}

get_error_stats <- function() {
  return(list(
    total_errors = 0,
    data_errors = 0,
    filter_errors = 0,
    input_errors = 0,
    last_error = NULL
  ))
}

reset_error_stats <- function() {
  return(invisible(NULL))
}
