# Visualization Module - Plot Generation

# Load required libraries
library(ggplot2)
library(ggiraph)

# Generate response distribution plot
generate_response_distribution_plot <- function(df, free_text_questions) {
  if (!is.null(df)) {
    # Count responses for each question
    response_counts <- sapply(names(free_text_questions), function(question) {
      if (question %in% names(df)) {
        responses <- df[[question]]
        sum(!is.na(responses) & responses != "")
      } else {
        0
      }
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      question = names(free_text_questions),
      count = response_counts,
      question_label = free_text_questions,
      stringsAsFactors = FALSE
    )
    
    # Order by count
    plot_data <- plot_data[order(plot_data$count, decreasing = TRUE), ]
    
    # Create interactive bar plot with ggiraph
    p <- ggplot(plot_data, aes(x = reorder(question_label, -count), y = count, fill = count)) +
      geom_col_interactive(
        aes(
          tooltip = paste(question_label, ": ", count, " responses"),
          data_id = question
        ),
        width = 0.7
      ) +
      scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Question", y = "Number of Responses", title = "") +
      coord_flip()
    
    # Return the plot and plot data
    return(list(plot = p, data = plot_data))
  } else {
    # Return empty plot if no data
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    
    return(list(plot = p, data = NULL))
  }
}

# Generate response length plot
generate_response_length_plot <- function(df, free_text_questions) {
  if (!is.null(df)) {
    # Calculate average response length for each question
    avg_lengths <- sapply(names(free_text_questions), function(question) {
      if (question %in% names(df)) {
        responses <- df[[question]]
        responses <- responses[!is.na(responses) & responses != ""]
        if (length(responses) > 0) {
          mean(nchar(responses))
        } else {
          0
        }
      } else {
        0
      }
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      question = names(free_text_questions),
      avg_length = avg_lengths,
      question_label = free_text_questions,
      stringsAsFactors = FALSE
    )
    
    # Order by average length
    plot_data <- plot_data[order(plot_data$avg_length, decreasing = TRUE), ]
    
    # Create interactive bar plot with ggiraph
    p <- ggplot(plot_data, aes(x = reorder(question_label, -avg_length), y = avg_length, fill = avg_length)) +
      geom_col_interactive(
        aes(
          tooltip = paste(question_label, ": ", round(avg_length, 1), " characters"),
          data_id = question
        ),
        width = 0.7
      ) +
      scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Question", y = "Average Response Length (characters)", title = "") +
      coord_flip()
    
    # Return the plot and plot data
    return(list(plot = p, data = plot_data))
  } else {
    # Return empty plot if no data
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    
    return(list(plot = p, data = NULL))
  }
}