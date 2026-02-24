# R/error_handling.R
# Error handling utilities for the CPSC Experience Survey Explorer

# =============================================================================
# Error Handling Configuration
# =============================================================================

#' Get error message based on error type
#'
#' Provides user-friendly error messages for different error scenarios.
#'
#' @param error_type Character error type (default: "unknown")
#' @return Character error message
#' @export
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

# =============================================================================
# Error Handling Functions
# =============================================================================

#' Handle data loading errors gracefully
#'
#' Provides comprehensive error handling for data loading operations.
#'
#' @param data_file_path Character path to data file
#' @param error_type Character error type to handle
#' @return Logical indicating if error was handled successfully
#' @export
handle_data_error <- function(data_file_path, error_type = "unknown") {
  # Log the error
  log_error(error_type, data_file_path)
  
  # Show user-friendly notification
  show_error_notification(error_type)
  
  # Return FALSE to indicate error occurred
  return(FALSE)
}

#' Display user-friendly error notification
#'
#' Shows a toast-style notification with an error message.
#'
#' @param error_type Character error type
#' @param duration Numeric duration in seconds (default: ERROR_TOAST_DURATION)
#' @export
show_error_notification <- function(error_type = "unknown", duration = ERROR_TOAST_DURATION) {
  message <- getErrorMessage(error_type)
  
  showNotification(
    message,
    type = "error",
    duration = duration
  )
}

#' Log errors for debugging
#'
#' Logs errors to console or file based on configuration.
#'
#' @param error_type Character error type
#' @param context Character additional context information
#' @param level Character log level (default: "ERROR")
#' @export
log_error <- function(error_type = "unknown", context = NULL, level = "ERROR") {
  # Check if logging is enabled
  if (LOG_LEVEL == "OFF") {
    return(invisible(NULL))
  }
  
  # Determine if this error should be logged
  if (level != "ERROR" && LOG_LEVEL != "DEBUG") {
    return(invisible(NULL))
  }
  
  # Create log message
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] [%s] %s", timestamp, level, error_type)
  
  if (!is.null(context)) {
    log_entry <- paste0(log_entry, " - ", context)
  }
  
  # Log to console
  if (LOG_TO_CONSOLE) {
    message(log_entry)
  }
  
  # Log to file
  if (LOG_TO_FILE) {
    if (!dir.exists(dirname(LOG_FILE_PATH))) {
      dir.create(dirname(LOG_FILE_PATH), recursive = TRUE)
    }
    write(log_entry, file = LOG_FILE_PATH, append = TRUE)
  }
  
  return(invisible(NULL))
}

#' Validate user inputs
#'
#' Validates user inputs against expected criteria.
#'
#' @param input_value Any input value to validate
#' @param validation_type Character validation type (default: "required")
#' @param options Character vector of valid options (for type "options")
#' @return Logical TRUE if valid, FALSE otherwise
#' @export
validate_inputs <- function(input_value, validation_type = "required", options = NULL) {
  # Check if input is required
  if (validation_type == "required") {
    if (is.null(input_value) || input_value == "") {
      show_error_notification("input_invalid")
      return(FALSE)
    }
  }
  
  # Check if input is in valid options
  if (validation_type == "options" && !is.null(options)) {
    if (!(input_value %in% options)) {
      show_error_notification("filter_invalid")
      return(FALSE)
    }
  }
  
  # Check if input is numeric
  if (validation_type == "numeric") {
    if (!is.numeric(input_value) || is.na(input_value)) {
      show_error_notification("input_invalid")
      return(FALSE)
    }
  }
  
  # Check if input is within range
  if (validation_type == "range") {
    if (is.null(input_value) || is.na(input_value)) {
      show_error_notification("input_invalid")
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Safe reactive expression wrapper
#'
#' Wraps reactive expressions with comprehensive error handling.
#'
#' @param reactive_expr Reactive expression to wrap
#' @param error_handler Function to call on error (optional)
#' @return Reactive expression with error handling
#' @export
safe_reactive <- function(reactive_expr, error_handler = NULL) {
  return(reactive({
    tryCatch({
      result <- reactive_expr()
      
      # Check if result is valid
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

#' Safe function wrapper
#'
#' Wraps regular functions with error handling.
#'
#' @param func Function to wrap
#' @param error_handler Function to call on error (optional)
#' @return Wrapped function with error handling
#' @export
safe_function <- function(func, error_handler = NULL) {
  return(function(...) {
    tryCatch({
      result <- func(...)
      
      # Check if result is valid
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

#' Show warning notification
#'
#' Displays a warning-style notification to the user.
#'
#' @param message Character warning message
#' @param duration Numeric duration in seconds (default: WARNING_TOAST_DURATION)
#' @export
show_warning_notification <- function(message, duration = WARNING_TOAST_DURATION) {
  showNotification(
    message,
    type = "warning",
    duration = duration
  )
}

#' Show success notification
#'
#' Displays a success-style notification to the user.
#'
#' @param message Character success message
#' @param duration Numeric duration in seconds (default: ERROR_TOAST_DURATION)
#' @export
show_success_notification <- function(message, duration = ERROR_TOAST_DURATION) {
  showNotification(
    message,
    type = "message",
    duration = duration
  )
}

#' Check data file status
#'
#' Checks if data file exists and is valid.
#'
#' @param data_file_path Character path to data file
#' @return List with status and message
#' @export
check_data_file <- function(data_file_path) {
  # Check if file exists
  if (!file.exists(data_file_path)) {
    return(list(
      status = "error",
      message = "Data file not found",
      error_type = "data_not_found"
    ))
  }
  
  # Check if file is readable
  if (!file.info(data_file_path)$isfile) {
    return(list(
      status = "error",
      message = "Data file is not a valid file",
      error_type = "data_corrupted"
    ))
  }
  
  # Check file size
  file_size <- file.info(data_file_path)$size
  if (file_size == 0) {
    return(list(
      status = "error",
      message = "Data file is empty",
      error_type = "data_empty"
    ))
  }
  
  # Check if file is readable
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

#' Get error statistics
#'
#' Returns summary of error handling statistics.
#'
#' @return List of error statistics
#' @export
get_error_stats <- function() {
  # This would typically track errors in a real application
  # For now, return placeholder values
  return(list(
    total_errors = 0,
    data_errors = 0,
    filter_errors = 0,
    input_errors = 0,
    last_error = NULL
  ))
}

#' Reset error tracking
#'
#' Resets error tracking statistics.
#'
#' @export
reset_error_stats <- function() {
  # This would typically reset tracking in a real application
  return(invisible(NULL))
}
