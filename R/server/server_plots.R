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

# Generate learning preference plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_learning_preference_plot <- function(df) {
  plot_result <- generate_learning_preference_plot(df)
  return(plot_result$plot)
}

# Generate prior experience plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_prior_experience_plot <- function(df) {
  plot_result <- generate_prior_experience_plot(df)
  return(plot_result$plot)
}

# Generate course satisfaction plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_course_satisfaction_plot <- function(df) {
  plot_result <- generate_course_satisfaction_plot(df)
  return(plot_result$plot)
}

# Generate Discord engagement plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_discord_engagement_plot <- function(df) {
  plot_result <- generate_discord_engagement_plot(df)
  return(plot_result$plot)
}

# Generate learning methods plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_learning_methods_plot <- function(df) {
  plot_result <- generate_learning_methods_plot(df)
  return(plot_result$plot)
}

# Generate community connection plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_community_connection_plot <- function(df) {
  plot_result <- generate_community_connection_plot(df)
  return(plot_result$plot)
}

# Generate section comparison plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_section_comparison_plot <- function(df) {
  plot_result <- generate_section_comparison_plot(df)
  return(plot_result$plot)
}

# Generate section breakdown pie chart
# Returns a ggplot object with interactive elements for ggiraph rendering
get_section_breakdown_plot <- function(df) {
  plot_result <- generate_section_breakdown_plot(df)
  return(plot_result$plot)
}

# Generate key themes plot
# Returns a ggplot object with interactive elements for ggiraph rendering
get_key_themes_plot <- function(df) {
  plot_result <- generate_key_themes_plot(df)
  return(plot_result$plot)
}