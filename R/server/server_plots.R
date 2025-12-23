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

# These functions are no longer needed since we're using ggiraph for interactive charts
# The click handling is now handled natively by ggiraph in the server_main.R file