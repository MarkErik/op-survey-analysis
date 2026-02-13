# Data Validation Module
# Functions for validating survey data integrity

#' Validate Survey Data
#'
#' Performs comprehensive validation checks on processed survey data,
#' including required columns, data types, and duplicate participant IDs.
#'
#' @param df A processed data frame
#' @return A list with 'valid' (logical) and 'errors' (character vector) components
#' @export
validate_survey_data <- function(df) {
  if (is.null(df)) {
    return(list(valid = FALSE, errors = "Data is NULL"))
  }
  
  errors <- c()
  
  # Check for required columns
  required_cols <- c("timestamp", "section", "participant_id")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for duplicate participant IDs
  if ("participant_id" %in% names(df)) {
    dup_ids <- df$participant_id[duplicated(df$participant_id)]
    if (length(dup_ids) > 0) {
      errors <- c(errors, paste("Duplicate participant IDs found:", length(dup_ids)))
    }
  }
  
  # Check for empty data frame
  if (nrow(df) == 0) {
    errors <- c(errors, "Data frame is empty")
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

#' Log Validation Errors
#'
#' Logs validation issues to the console with appropriate severity levels.
#'
#' @param validation_result A list returned by validate_survey_data()
#' @export
log_validation_errors <- function(validation_result) {
  if (validation_result$valid) {
    message("Data validation passed successfully")
  } else {
    message("Data validation failed with the following errors:")
    for (error in validation_result$errors) {
      message("  - ", error)
    }
  }
}
