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
      pageLength = 30,
      scrollX = TRUE,
      searching = TRUE,
      ordering = TRUE,
      info = TRUE,
      autoWidth = TRUE,
      dom = 'tip',  # Hide "Show X entries" dropdown (t=table, i=info, p=pagination)
      columnDefs = list(list(visible = FALSE, targets = 0))  # Hide response_id column
    ),
    selection = 'single',
    rownames = FALSE,
    caption = paste("Responses to:", free_text_questions[current_question()])
  )
}

# Handle row selection in responses table
handle_row_selection <- function(input, selected_response_id, current_responses, session) {
  observeEvent(input$responses_table_rows_selected, {
    if (!is.null(input$responses_table_rows_selected) && length(input$responses_table_rows_selected) > 0) {
      row_idx <- input$responses_table_rows_selected[1]
      # Extract response_id from the selected row
      selected_response_id(current_responses()[row_idx, "response_id"])
    }
  })
}

# Render participant profile
render_participant_profile <- function(selected_response_id, current_responses, current_question, df, free_text_questions) {
  if (is.null(selected_response_id()) || is.null(current_responses())) {
    return(p("No participant selected. Click on a response in the table to view the participant's profile."))
  }
  
  # Get the selected participant's data
  participant_data <- get_participant_profile(df, selected_response_id())
  
  if (is.null(participant_data)) {
    return(p("Error loading participant data."))
  }
  
  # Create the profile UI - pass the actual value of current_question
  create_participant_profile_ui(participant_data, current_question(), free_text_questions)
}