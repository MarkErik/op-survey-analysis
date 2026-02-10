# Utility Functions Module

# Convert text responses to numeric scores (1-5)
# This function handles Likert-type responses by extracting numeric values
# from various formats like "5 - Strongly Agree", "4", "3", etc.
convert_to_numeric <- function(responses, scale = NULL) {
  # Remove all non-numeric characters to extract the score
  # Handles formats like "5 - Strongly Agree", "4", "3", etc.
  numeric_values <- sapply(responses, function(r) {
    if (is.na(r) || r == "") {
      return(NA)
    }
    
    # Extract numeric value by removing all non-numeric characters
    numeric_value <- as.numeric(gsub("[^0-9]", "", r))
    
    return(numeric_value)
  })
  
  return(as.numeric(numeric_values))
}

# Format response for display
format_response <- function(response) {
  if (is.null(response) || is.na(response) || response == "") {
    return("No response provided")
  }
  
  return(response)
}

# Create participant profile UI
create_participant_profile_ui <- function(participant_data, current_question, free_text_questions) {
  if (is.null(participant_data)) {
    return(p("Error loading participant data."))
  }
  
  # Format the profile display
  profile_html <- div(class = "profile-container",
    div(class = "profile-header",
      h4("Participant Information"),
      p(paste("Selected response from:", free_text_questions[current_question()]))
    ),
    
    div(class = "profile-content",
      # Basic information
      fluidRow(
        column(6,
          h5("Basic Information"),
          tags$ul(
            tags$li(strong("Section:"), participant_data$section),
            tags$li(strong("Prior Experience:"), participant_data$prior_experience),
            tags$li(strong("Learning Preference:"), participant_data$learning_preference)
          )
        ),
        column(6,
          h5("Selected Response"),
          div(class = "response-text",
            p(format_response(participant_data[[current_question()]]))
          )
        )
      ),
      
      hr(),
      
      # All other responses
      h5("All Other Responses"),
      div(class = "all-responses",
        lapply(names(free_text_questions), function(question) {
          if (question != current_question()) {
            div(class = "response-item",
              h6(free_text_questions[question]),
              p(format_response(participant_data[[question]]))
            )
          }
        })
      )
    )
  )
  
  return(profile_html)
}