# Data Import Module
# Functions for loading survey data from CSV files

#' Load Survey Data
#'
#' Loads the CPSC Experience Survey CSV file with proper quote handling
#' and error handling for missing or empty files.
#'
#' @return A data frame containing the raw survey data, or NULL on failure
#' @export
load_survey_data <- function() {
  tryCatch({
    # Define the survey file path
    survey_file <- "survey_data/CPSC Experience Survey.csv"
    
    # Check if file exists
    if (!file.exists(survey_file)) {
      stop("Survey data file not found: ", survey_file)
    }
    
    # Read CSV with proper quote handling for multi-line values
    df <- read.csv(
      survey_file,
      stringsAsFactors = FALSE,
      quote = "\"",
      comment.char = ""
    )
    
    # Check if data frame is empty
    if (nrow(df) == 0) {
      stop("Survey data file is empty")
    }
    
    message("Successfully loaded ", nrow(df), " survey responses")
    return(df)
    
  }, error = function(e) {
    message("ERROR loading survey data: ", e$message)
    return(NULL)
  })
}
