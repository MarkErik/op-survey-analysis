# Test script for verifying the changes to the survey analysis app

# Load required libraries
library(tidyverse)
library(stringr)

# Source the global R file to load the functions
source("../R/global.R")

# Create a test data frame that mimics the structure of the exported data
create_test_data <- function() {
  # Create sample data with all the required columns
  test_data <- data.frame(
    timestamp = as.character(Sys.time() - (1:10)*86400),  # 10 timestamps, one per day
    section = rep(c("A", "B"), 5),
    prior_experience = rep(c("Yes", "No", "Some"), c(3, 4, 3)),
    learning_preference = rep(c("Visual", "Auditory", "Kinesthetic"), c(4, 3, 3)),
    free_text_learning_preference = c(
      "I prefer visual learning with diagrams and charts.",
      "I learn best by listening to lectures and discussions.",
      paste0("This is a very long response that exceeds 200 characters to test if the truncation has been properly removed. ",
             "It contains multiple sentences and should be displayed in full without any truncation in the participant profile. ",
             "This ensures that users can see the complete response without missing any important information."),
      "I like hands-on activities and practical examples.",
      "Visual aids help me understand complex concepts better.",
      "I prefer group discussions and collaborative learning.",
      paste0("Another long response to test the truncation removal. This response is also designed to exceed the 200 character limit ",
             "that was previously in place. By removing this limit, we ensure that all responses are displayed in their entirety, ",
             "providing a complete view of participant feedback without any omissions."),
      "I learn best through reading and taking notes.",
      "Auditory learning works best for me, especially with explanations.",
      "I prefer a mix of different learning styles depending on the topic."
    ),
    free_text_challenge_meeting_people = rep("Sample challenge response", 10),
    free_text_class_welcoming_inclusive = rep("Sample inclusive response", 10),
    stringsAsFactors = FALSE
  )
  
  return(test_data)
}

# Test 1: Verify get_responses_for_question removes the specified columns
test_get_responses_for_question <- function() {
  cat("Testing get_responses_for_question function...\n")
  
  # Create test data
  test_df <- create_test_data()
  
  # Get responses for a question
  question <- "free_text_learning_preference"
  responses <- get_responses_for_question(test_df, question)
  
  # Check if the columns are removed
  columns_to_check <- c("timestamp", "row_id", "response_length")
  removed_columns <- columns_to_check[!columns_to_check %in% names(responses)]
  
  if (length(removed_columns) == length(columns_to_check)) {
    cat("✓ PASS: All specified columns (timestamp, row_id, response_length) have been removed.\n")
  } else {
    cat("✗ FAIL: Not all specified columns were removed. Missing:", 
        paste(columns_to_check[columns_to_check %in% names(responses)], collapse = ", "), "\n")
  }
  
  # Check if expected columns are present
  expected_columns <- c("section", "prior_experience", "learning_preference", "response")
  missing_columns <- expected_columns[!expected_columns %in% names(responses)]
  
  if (length(missing_columns) == 0) {
    cat("✓ PASS: All expected columns are present.\n")
  } else {
    cat("✗ FAIL: Missing expected columns:", paste(missing_columns, collapse = ", "), "\n")
  }
  
  cat("\n")
  
  return(responses)
}

# Test 2: Verify get_participant_profile works with the new approach
test_get_participant_profile <- function() {
  cat("Testing get_participant_profile function...\n")
  
  # Create test data
  test_df <- create_test_data()
  
  # Get responses for a question
  question <- "free_text_learning_preference"
  responses <- get_responses_for_question(test_df, question)
  
  # Test getting a participant profile
  if (nrow(responses) > 0) {
    row_id <- 1  # Get the first response
    profile <- get_participant_profile(test_df, row_id, responses, question)
    
    if (!is.null(profile) && nrow(profile) > 0) {
      cat("✓ PASS: Participant profile retrieved successfully.\n")
      
      # Check if the profile contains the expected response
      expected_response <- responses$response[row_id]
      actual_response <- profile[[question]]
      
      if (expected_response == actual_response) {
        cat("✓ PASS: Profile contains the correct response.\n")
      } else {
        cat("✗ FAIL: Profile does not contain the correct response.\n")
      }
    } else {
      cat("✗ FAIL: Failed to retrieve participant profile.\n")
    }
  } else {
    cat("✗ FAIL: No responses available for testing.\n")
  }
  
  cat("\n")
}

# Test 3: Verify format_response does not truncate long responses
test_format_response <- function() {
  cat("Testing format_response function...\n")
  
  # Create a long response (> 200 characters)
  long_response <- paste0("This is a very long response that exceeds 200 characters to test if the truncation has been properly removed. ",
                         "It contains multiple sentences and should be displayed in full without any truncation in the participant profile. ",
                         "This ensures that users can see the complete response without missing any important information.")
  
  # Format the response
  formatted_response <- format_response(long_response)
  
  # Check if the response is truncated
  if (nchar(formatted_response) == nchar(long_response)) {
    cat("✓ PASS: Long response is not truncated.\n")
  } else {
    cat("✗ FAIL: Long response is truncated. Original length:", nchar(long_response), 
        "Formatted length:", nchar(formatted_response), "\n")
  }
  
  # Check if the response ends with "..." (indicating truncation)
  if (!grepl("\\.\\.\\.$", formatted_response)) {
    cat("✓ PASS: Response does not end with '...'.\n")
  } else {
    cat("✗ FAIL: Response ends with '...', indicating truncation.\n")
  }
  
  # Test with empty response
  empty_response <- format_response("")
  if (empty_response == "No response provided") {
    cat("✓ PASS: Empty response handled correctly.\n")
  } else {
    cat("✗ FAIL: Empty response not handled correctly.\n")
  }
  
  # Test with NULL response
  null_response <- format_response(NULL)
  if (null_response == "No response provided") {
    cat("✓ PASS: NULL response handled correctly.\n")
  } else {
    cat("✗ FAIL: NULL response not handled correctly.\n")
  }
  
  cat("\n")
}

# Run all tests
run_all_tests <- function() {
  cat("Running tests for survey analysis app changes...\n")
  cat("==============================================\n\n")
  
  # Test 1
  responses <- test_get_responses_for_question()
  
  # Test 2
  test_get_participant_profile()
  
  # Test 3
  test_format_response()
  
  cat("All tests completed.\n")
}

# Execute the tests
run_all_tests()