# Data Processing Module

# Generate unique response IDs
generate_response_ids <- function(n) {
  paste0("RPT-", sprintf("%04d", 1:n))
}

# Load data
load_data <- function() {
  tryCatch({
    df <- read.csv("survey_data/exported_data.csv", stringsAsFactors = FALSE)
    # Clean column names
    names(df) <- tolower(names(df))
    # Remove any leading/trailing whitespace from character columns
    df <- df %>% mutate(across(where(is.character), str_trim))
    # Add response_id as first column
    df$response_id <- generate_response_ids(nrow(df))
    df <- df %>% select(response_id, everything())
    return(df)
  }, error = function(e) {
    message("Error loading data: ", e$message)
    return(NULL)
  })
}

# Get all responses for a specific free-text question
get_responses_for_question <- function(df, question) {
  if (is.null(df) || !question %in% names(df)) {
    return(NULL)
  }
  
  responses <- df %>%
    filter(!is.na(!!sym(question)) & !!sym(question) != "") %>%
    select(all_of(c("response_id", "prior_experience", "learning_preference", "section", question))) %>%
    rename(response = all_of(question))
  
  return(responses)
}

# Get participant profile
get_participant_profile <- function(df, response_id) {
  if (is.null(df) || is.null(response_id)) {
    return(NULL)
  }
  
  # Direct ID lookup - no content matching needed
  profile <- df %>%
    filter(response_id == response_id) %>%
    slice(1)
  
  if (nrow(profile) == 0) {
    return(NULL)
  }
  
  return(profile)
}

# Calculate statistics for the home page
calculate_statistics <- function(df, free_text_questions) {
  if (is.null(df)) {
    return(list(
      total_responses = 0,
      question_count = length(free_text_questions)
    ))
  }
  
  return(list(
    total_responses = nrow(df),
    question_count = length(free_text_questions)
  ))
}