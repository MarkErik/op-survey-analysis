# Utility Functions Module

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

# Handle plot click events
handle_plot_click <- function(click_data, plot_data, current_question, current_responses, selected_row, df, session, free_text_questions) {
  if (!is.null(click_data)) {
    # Get the y-coordinate (which corresponds to the question in a flipped plot)
    y_coord <- round(click_data$y)
    
    if (!is.null(plot_data)) {
      # The y-coordinates in a flipped plot correspond to the row numbers (1-based)
      if (y_coord >= 1 && y_coord <= nrow(plot_data)) {
        question_key <- plot_data$question[y_coord]
        
        if (!is.null(question_key) && question_key %in% names(free_text_questions)) {
          # Set the current question and responses
          current_question(question_key)
          current_responses(get_responses_for_question(df, question_key))
          selected_row(NULL)
          
          # Switch to Question Responses tab
          updateTabsetPanel(session, "tabset", selected = "Question Responses")
        }
      }
    }
  }
}

# Handle JavaScript click events
handle_js_click <- function(click_data, current_question, current_responses, selected_row, df, session, free_text_questions) {
  if (!is.null(click_data) && !is.null(click_data$question)) {
    question_key <- click_data$question
    
    # Set the current question and responses
    current_question(question_key)
    current_responses(get_responses_for_question(df, question_key))
    selected_row(NULL)
    
    # Switch to Question Responses tab
    updateTabsetPanel(session, "tabset", selected = "Question Responses")
  }
}