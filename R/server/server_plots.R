# Server Module - Plot Handling
#
# This module provides wrapper functions for generating interactive plots using ggiraph.
# The plots use geom_col_interactive with tooltip and data_id parameters to enable
# click detection in Shiny via input$<plot_id>_selected.

# Generate response distribution plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_response_distribution_plot <- function(df, free_text_questions) {
  plot_result <- generate_response_distribution_plot(df, free_text_questions)
  return(plot_result$plot)
}

# Generate response length plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_response_length_plot <- function(df, free_text_questions) {
  plot_result <- generate_response_length_plot(df, free_text_questions)
  return(plot_result$plot)
}