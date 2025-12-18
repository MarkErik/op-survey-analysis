# Global variables and data loading

# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(stringr)

# Define free-text questions
free_text_questions <- c(
  "free_text_learning_preference" = "Learning Preference",
  "free_text_challenge_meeting_people" = "Challenge Meeting People",
  "free_text_class_welcoming_inclusive" = "Class Welcoming & Inclusive",
  "free_text_class_interesting_engaging" = "Class Interesting & Engaging",
  "free_text_learning_meeting_expectations" = "Learning Meeting Expectations",
  "free_text_hopes_interacting_students" = "Hopes Interacting with Students",
  "free_text_hopes_interacting_professor" = "Hopes Interacting with Professor",
  "free_text_class_favorite_part" = "Class Favorite Part",
  "free_text_class_least_enjoyable_part" = "Class Least Enjoyable Part",
  "free_text_more_and_less_of" = "More and Less Of",
  "discord_other_text" = "Discord Other Text"
)

# Load data
load_data <- function() {
  tryCatch({
    df <- read.csv("exported_data.csv", stringsAsFactors = FALSE)
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
    select(all_of(c("timestamp", "section", "prior_experience", "learning_preference", question))) %>%
    rename(response = question) %>%
    mutate(
      row_id = row_number(),
      response_length = nchar(response)
    )
  
  return(responses)
}

# Get participant profile
get_participant_profile <- function(df, row_id) {
  if (is.null(df) || is.na(row_id) || is.null(current_responses())) {
    return(NULL)
  }
  
  # Get the timestamp from the selected row in the filtered responses
  selected_timestamp <- current_responses()$timestamp[row_id]
  
  if (is.null(selected_timestamp)) {
    return(NULL)
  }
  
  # Find the matching row in the original data frame
  profile <- df %>%
    filter(timestamp == selected_timestamp) %>%
    select(-starts_with("discord_")) %>%
    mutate(across(where(is.character), str_trim)) %>%
    slice(1)  # In case of duplicates, take the first one
  
  return(profile)
}

# Format response for display
format_response <- function(response) {
  if (is.null(response) || is.na(response) || response == "") {
    return("No response provided")
  }
  
  # Truncate long responses for preview
  if (nchar(response) > 200) {
    return(paste0(substr(response, 1, 200), "..."))
  }
  
  return(response)
}