# Global variables and data loading

# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(stringr)
library(ggiraph)

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

# Define standardized column names for survey questions
# These match the lowercase-converted column names from the CSV file
survey_columns <- list(
  course = c(
    excited = "(course) i am excited about the content and material that i'm learning",
    relevant = "(course) the content is relevant and up-to-date",
    meeting_goals = "(course) i feel like i am meeting the goals of learning python in this course",
    apply_scenario = "(course) i feel like i could take what i'm learning and apply it in a new scenario",
    feedback = "(course) i'm satisfied with the level of feedback i receive",
    ask_help = "(course) it's easy to ask for help"
  ),
  learning = c(
    pre_written_code = "(learning) explanations of pre-written code",
    live_coding = "(learning) live coding by the professor",
    slides = "(learning) presentation slides",
    handouts = "(learning) post-class handouts and notes",
    tophat_quizzes = "(learning) tophat quizzes",
    assignments = "(learning) assignments",
    labs = "(learning) labs",
    ask_questions = "(learning) being able to ask questions of the professor during lecture",
    studying_midterms = "(learning) studying for midterms",
    coding_own = "(learning) coding on my own"
  ),
  community = c(
    friends_important = "(community) making friends within the class is important to me",
    easy_meet = "(community) it's easy to meet new people within the class",
    part_of_class = "(community) i feel like i am a part of this class",
    comfortable_speaking = "(community) i feel comfortable speaking up in class",
    part_of_university = "(community) i feel like i am a part of the university community"
  )
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
    select(all_of(c("section", "prior_experience", "learning_preference", question))) %>%
    rename(response = all_of(question))
  
  return(responses)
}

# Get participant profile
get_participant_profile <- function(df, row_id, responses_data = NULL, current_question = NULL) {
  if (is.null(df) || is.na(row_id) || is.null(responses_data) || is.null(current_question)) {
    return(NULL)
  }
  
  # Since we removed timestamp and row_id, we'll use the row index directly
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
  
  # Find the matching row in the original data frame
  # Only remove Discord columns if the current question is not a Discord question
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

# Format response for display
format_response <- function(response) {
  if (is.null(response) || is.na(response) || response == "") {
    return("No response provided")
  }
  
  return(response)
}