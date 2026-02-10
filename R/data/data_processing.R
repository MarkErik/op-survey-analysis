# Data Processing Module

# Load data
load_data <- function() {
  tryCatch({
    df <- read.csv("survey_data/exported_data.csv", stringsAsFactors = FALSE)
    # Clean column names
    names(df) <- tolower(names(df))
    # Remove any leading/trailing whitespace from character columns
    df <- df %>% mutate(across(where(is.character), str_trim))
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
    select(all_of(c("section", "prior_experience", "learning_preference", question))) %>%
    rename(response = all_of(question))
  
  return(responses)
}

# Get participant profile
get_participant_profile <- function(df, row_id, responses_data = NULL, current_question = NULL) {
  if (is.null(df) || is.na(row_id) || is.null(responses_data) || is.null(current_question)) {
    return(NULL)
  }
   
  # Get the selected row from the filtered responses
  selected_response <- responses_data[row_id, ]
  
  if (nrow(selected_response) == 0) {
    return(NULL)
  }
  
  # Find the matching row in the original data frame using the response content
  # This assumes the response content is unique enough to identify the participant
  response_content <- selected_response$response
  
  if (is.null(response_content) || response_content == "") {
    return(NULL)
  }
  
  if (startsWith(current_question, "discord_")) {
    profile <- df %>%
      filter(!!sym(current_question) == response_content) %>%
      mutate(across(where(is.character), str_trim)) %>%
      slice(1)  # In case of duplicates, take the first one
  } else {
    profile <- df %>%
      filter(!!sym(current_question) == response_content) %>%
      select(-starts_with("discord_")) %>%
      mutate(across(where(is.character), str_trim)) %>%
      slice(1)  # In case of duplicates, take the first one
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