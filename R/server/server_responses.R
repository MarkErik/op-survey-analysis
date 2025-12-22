# Server Module - Response Handling

# Render responses table
render_responses_table <- function(current_responses, free_text_questions, current_question) {
  if (is.null(current_responses())) {
    return(NULL)
  }
  
  responses <- current_responses()
  
  datatable(
    responses,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      scrollY = "400px",
      searching = TRUE,
      ordering = TRUE,
      info = TRUE,
      autoWidth = TRUE
    ),
    selection = 'single',
    rownames = FALSE,
    caption = paste("Responses to:", free_text_questions[current_question()])
  )
}

# Handle row selection in responses table
handle_row_selection <- function(input, selected_row, session) {
  observeEvent(input$responses_table_rows_selected, {
    if (!is.null(input$responses_table_rows_selected) && length(input$responses_table_rows_selected) > 0) {
      selected_row(input$responses_table_rows_selected[1])
      
      # Switch to Participant Profile tab
      updateTabsetPanel(session, "tabset", selected = "Participant Profile")
    }
  })
}

# Render participant profile
render_participant_profile <- function(selected_row, current_responses, current_question, df, free_text_questions) {
  if (is.null(selected_row()) || is.null(current_responses())) {
    return(p("No participant selected. Click on a response in the table to view the participant's profile."))
  }
  
  # Get the selected participant's data
  participant_data <- get_participant_profile(df, selected_row(), current_responses(), current_question())
  
  if (is.null(participant_data)) {
    return(p("Error loading participant data."))
  }
  
  # Create the profile UI
  create_participant_profile_ui(participant_data, current_question, free_text_questions)
}