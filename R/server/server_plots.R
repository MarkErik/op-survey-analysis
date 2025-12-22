# Server Module - Plot Handling

# Generate and render response distribution plot
render_response_distribution_plot <- function(df, free_text_questions, plot_data_dist) {
  # Generate the plot
  plot_result <- generate_response_distribution_plot(df, free_text_questions)
  
  # Store the plot data for click handling
  if (!is.null(plot_result$data)) {
    plot_data_dist(plot_result$data)
  }
  
  # Return the plot
  plot_result$plot
}

# Generate and render response length plot
render_response_length_plot <- function(df, free_text_questions, plot_data_length) {
  # Generate the plot
  plot_result <- generate_response_length_plot(df, free_text_questions)
  
  # Store the plot data for click handling
  if (!is.null(plot_result$data)) {
    plot_data_length(plot_result$data)
  }
  
  # Return the plot
  plot_result$plot
}

# Handle click events on response distribution plot
handle_dist_plot_click <- function(input, current_question, current_responses, selected_row, df, plot_data_dist, session, free_text_questions) {
  observeEvent(input$dist_plot_click, {
    click_data <- input$dist_plot_click
    handle_plot_click(click_data, plot_data_dist(), current_question, current_responses, selected_row, df, session, free_text_questions)
  })
}

# Handle click events on response length plot
handle_length_plot_click <- function(input, current_question, current_responses, selected_row, df, plot_data_length, session, free_text_questions) {
  observeEvent(input$length_plot_click, {
    click_data <- input$length_plot_click
    handle_plot_click(click_data, plot_data_length(), current_question, current_responses, selected_row, df, session, free_text_questions)
  })
}

# Handle bar click events from JavaScript
handle_bar_click <- function(input, current_question, current_responses, selected_row, df, session, free_text_questions) {
  observeEvent(input$bar_click, {
    click_data <- input$bar_click
    handle_js_click(click_data, current_question, current_responses, selected_row, df, session, free_text_questions)
  })
}

# Handle y-axis click events from JavaScript
handle_y_axis_click <- function(input, current_question, current_responses, selected_row, df, session, free_text_questions) {
  observeEvent(input$y_axis_click, {
    click_data <- input$y_axis_click
    handle_js_click(click_data, current_question, current_responses, selected_row, df, session, free_text_questions)
  })
}

# Send plot data to JavaScript when requested
send_plot_data_to_js <- function(input, session, plot_data_dist, plot_data_length) {
  # Send distribution plot data
  observeEvent(input$get_dist_plot_data, {
    dist_data <- plot_data_dist()
    if (!is.null(dist_data)) {
      # Create a mapping of question keys to labels as a list
      question_mapping <- as.list(dist_data$question_label)
      names(question_mapping) <- dist_data$question
      
      # Send the data to JavaScript
      session$sendCustomMessage("dist_plot_data", question_mapping)
    }
  })
  
  # Send length plot data
  observeEvent(input$get_length_plot_data, {
    length_data <- plot_data_length()
    if (!is.null(length_data)) {
      # Create a mapping of question keys to labels as a list
      question_mapping <- as.list(length_data$question_label)
      names(question_mapping) <- length_data$question
      
      # Send the data to JavaScript
      session$sendCustomMessage("length_plot_data", question_mapping)
    }
  })
}