# Global variables and data loading

# Load required packages
library(dplyr)
library(stringr)
library(tidyr)

# Initialize global column mappings
column_mappings <- list(
  original = character(0),
  normalized = character(0)
)

# Source data modules
source("R/data/data_import.R")
source("R/data/data_processing.R")
source("R/data/data_validation.R")
source("R/data/data_access.R")

# Load and process survey data at app startup
raw_data <- load_survey_data()

if (!is.null(raw_data)) {
  survey_data <- process_survey_data(raw_data)
  
  # Validate processed data
  validation_result <- validate_survey_data(survey_data)
  log_validation_errors(validation_result)
  
  if (!validation_result$valid) {
    message("WARNING: Data validation failed. Some features may not work correctly.")
  }
} else {
  survey_data <- NULL
  message("ERROR: Failed to load survey data. Application may not function correctly.")
}

# Define free-text questions for reference
free_text_questions <- c(
  "how_is_the_course_meeting_your_expectations_for_what_you_hoped_to_learn_or_experience_optional",
  "why_is_this_your_preferred_way_of_learning",
  "optional_if_you_re_not_taking_this_class_in_your_preferred_learning_method_why",
  "thinking_about_what_helps_you_learn_the_best",
  "what_s_been_your_favorite_part_of_the_class_optional",
  "what_s_been_the_least_enjoyable_part_optional",
  "what_s_the_greatest_challenge_in_meeting_new_people_optional",
  "please_remark_on_aspects_of_the_class_that_make_it_welcoming_optional",
  "what_were_your_expectations_hopes_for_interacting_with_the_other_students_optional",
  "what_were_your_expectations_hopes_for_interacting_with_the_professor_optional",
  "any_other_comments_optional"
)
