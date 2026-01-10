# Visualization Module - Plot Generation
#
# This module generates interactive plots using ggiraph for the survey explorer application.
#
# Key Concepts:
# - geom_col_interactive: Creates interactive bar charts with hover tooltips and click detection
# - tooltip: Text displayed when hovering over a bar element
# - data_id: Unique identifier for each bar, used for click detection in Shiny (input$<plot_id>_selected)
#
# Usage in Shiny:
# 1. Generate plot with geom_col_interactive(aes(tooltip=..., data_id=question_id))
# 2. Render with renderGirafe() and girafe(ggobj=plot)
# 3. Handle clicks via observeEvent(input$<plot_id>_selected, {...})

# Load required libraries
library(ggplot2)
library(ggiraph)

# Generate response distribution plot
# Creates an interactive bar chart showing the number of responses per question
#
# Parameters:
#   df: Data frame containing survey responses
#   free_text_questions: Named list/vector mapping question IDs to question labels
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting (for reference/debugging)
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
    # - tooltip: Shows question label and response count on hover
    # - data_id: Question ID used for click detection (input$response_distribution_plot_selected)
    p <- ggplot(plot_data, aes(x = reorder(question_label, -count), y = count)) +
      geom_col_interactive(
        aes(
          tooltip = paste(question_label, ": ", count, " responses"),
          data_id = question
        ),
        fill = "#3498db",
        width = 0.7
      ) +
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
# Creates an interactive bar chart showing average response length per question
#
# Parameters:
#   df: Data frame containing survey responses
#   free_text_questions: Named list/vector mapping question IDs to question labels
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting (for reference/debugging)
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
    # - tooltip: Shows question label and average character count on hover
    # - data_id: Question ID used for click detection (input$response_length_plot_selected)
    p <- ggplot(plot_data, aes(x = reorder(question_label, -avg_length), y = avg_length)) +
      geom_col_interactive(
        aes(
          tooltip = paste(question_label, ": ", round(avg_length, 1), " characters"),
          data_id = question
        ),
        fill = "#3498db",
        width = 0.7
      ) +
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

# Generate learning preference distribution plot
# Creates a pie/donut chart showing distribution of learning preferences
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_learning_preference_plot <- function(df) {
  if (!is.null(df) && "learning_preference" %in% names(df)) {
    # Count learning preferences
    pref_counts <- table(df$learning_preference, useNA = "ifany")
    pref_df <- data.frame(
      preference = names(pref_counts),
      count = as.numeric(pref_counts),
      percentage = round(as.numeric(pref_counts) / sum(pref_counts) * 100, 1),
      stringsAsFactors = FALSE
    )
    
    # Remove NA if present
    pref_df <- pref_df[!is.na(pref_df$preference), ]
    
    # Create donut chart
    p <- ggplot(pref_df, aes(x = "", y = count, fill = preference)) +
      geom_col_interactive(
        aes(
          tooltip = paste(preference, ": ", count, " (", percentage, "%)", sep = ""),
          data_id = preference
        ),
        width = 1,
        color = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      ) +
      labs(fill = "Learning Preference")
    
    return(list(plot = p, data = pref_df))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate prior experience breakdown plot
# Creates a bar chart showing distribution of prior programming experience
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_prior_experience_plot <- function(df) {
  if (!is.null(df) && "prior_experience" %in% names(df)) {
    # Count prior experience levels
    exp_counts <- table(df$prior_experience, useNA = "ifany")
    exp_df <- data.frame(
      experience = names(exp_counts),
      count = as.numeric(exp_counts),
      percentage = round(as.numeric(exp_counts) / sum(exp_counts) * 100, 1),
      stringsAsFactors = FALSE
    )
    
    # Remove NA if present
    exp_df <- exp_df[!is.na(exp_df$experience), ]
    
    # Order by experience level
    exp_df$experience <- factor(exp_df$experience,
                                levels = c("No experience at all",
                                          "Took programming course before (either in school, or online tutorials)",
                                          "Highly experienced (comfortable writing own programs)"))
    exp_df <- exp_df[order(exp_df$experience), ]
    
    # Create horizontal bar chart
    p <- ggplot(exp_df, aes(x = experience, y = count)) +
      geom_col_interactive(
        aes(
          tooltip = paste(experience, ": ", count, " (", percentage, "%)", sep = ""),
          data_id = experience
        ),
        fill = "#2ecc71",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Prior Experience", y = "Count", title = "") +
      coord_flip()
    
    return(list(plot = p, data = exp_df))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate section breakdown pie chart
# Creates a pie chart showing the distribution of students across sections
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_section_breakdown_plot <- function(df) {
  if (!is.null(df) && "section" %in% names(df)) {
    # Count students per section
    section_counts <- table(df$section, useNA = "ifany")
    section_df <- data.frame(
      section = names(section_counts),
      count = as.numeric(section_counts),
      percentage = round(as.numeric(section_counts) / sum(section_counts) * 100, 1),
      stringsAsFactors = FALSE
    )
    
    # Remove NA if present
    section_df <- section_df[!is.na(section_df$section), ]
    
    # Create pie chart
    p <- ggplot(section_df, aes(x = "", y = count, fill = section)) +
      geom_col_interactive(
        aes(
          tooltip = paste("Section ", section, ": ", count, " students (", percentage, "%)", sep = ""),
          data_id = section
        ),
        width = 1,
        color = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      ) +
      labs(fill = "Section")
    
    return(list(plot = p, data = section_df))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate course satisfaction overview plot
# Creates a horizontal bar chart showing average scores for course satisfaction questions
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_course_satisfaction_plot <- function(df) {
  if (!is.null(df)) {
    # Course satisfaction question columns - use centralized mapping
    course_questions <- survey_columns$course
    
    # Calculate average scores for each question
    avg_scores <- sapply(course_questions, function(q) {
      if (q %in% names(df)) {
        responses <- df[[q]]
        responses <- responses[!is.na(responses)]
        if (length(responses) > 0) {
          # Convert Likert scale to numeric
          numeric_responses <- sapply(responses, function(r) {
            if (grepl("Strongly Disagree", r)) return(1)
            if (grepl("Disagree", r) && !grepl("Strongly", r)) return(2)
            if (grepl("Neutral", r)) return(3)
            if (grepl("Agree", r) && !grepl("Strongly", r)) return(4)
            if (grepl("Strongly Agree", r)) return(5)
            return(NA)
          })
          mean(numeric_responses, na.rm = TRUE)
        } else {
          NA
        }
      } else {
        NA
      }
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      question = gsub("^x\\.course\\.\\.", "", course_questions),
      avg_score = avg_scores,
      stringsAsFactors = FALSE
    )
    plot_data <- plot_data[!is.na(plot_data$avg_score), ]
    
    # Order by average score
    plot_data <- plot_data[order(plot_data$avg_score, decreasing = TRUE), ]
    
    # Create horizontal bar chart
    p <- ggplot(plot_data, aes(x = reorder(question, avg_score), y = avg_score)) +
      geom_col_interactive(
        aes(
          tooltip = paste(question, ": ", round(avg_score, 2), "/ 5.00", sep = ""),
          data_id = question
        ),
        fill = "#9b59b6",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Question", y = "Average Score (1-5)", title = "") +
      coord_flip() +
      ylim(0, 5)
    
    return(list(plot = p, data = plot_data))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate Discord engagement metrics plot
# Creates a grouped bar chart showing Discord usage metrics
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_discord_engagement_plot <- function(df) {
  if (!is.null(df)) {
    # Discord question columns
    discord_questions <- c(
      "discord_i_have_joined_the_class_discord",
      "discord_i_am_active_in_the_class_discord",
      "discord_it_is_really_useful_for_my_learning"
    )
    
    discord_labels <- c(
      "Joined Discord",
      "Active on Discord",
      "Finds Discord Useful"
    )
    
    # Calculate percentages for each metric
    percentages <- sapply(discord_questions, function(q) {
      if (q %in% names(df)) {
        responses <- df[[q]]
        responses <- responses[!is.na(responses)]
        if (length(responses) > 0) {
          sum(responses == 1, na.rm = TRUE) / length(responses) * 100
        } else {
          NA
        }
      } else {
        NA
      }
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      metric = discord_labels,
      percentage = percentages,
      stringsAsFactors = FALSE
    )
    plot_data <- plot_data[!is.na(plot_data$percentage), ]
    
    # Create horizontal bar chart
    p <- ggplot(plot_data, aes(x = metric, y = percentage)) +
      geom_col_interactive(
        aes(
          tooltip = paste(metric, ": ", round(percentage, 1), "%", sep = ""),
          data_id = metric
        ),
        fill = "#7289da",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Discord Metric", y = "Percentage (%)", title = "") +
      coord_flip() +
      ylim(0, 100)
    
    return(list(plot = p, data = plot_data))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate most valuable learning methods plot
# Creates a ranked bar chart showing top learning methods by average rating
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_learning_methods_plot <- function(df) {
  if (!is.null(df)) {
    # Learning method question columns - use centralized mapping
    learning_questions <- survey_columns$learning
    
    # Calculate average scores for each method
    avg_scores <- sapply(learning_questions, function(q) {
      if (q %in% names(df)) {
        responses <- df[[q]]
        responses <- responses[!is.na(responses)]
        if (length(responses) > 0) {
          # Convert scale to numeric
          numeric_responses <- sapply(responses, function(r) {
            if (grepl("Doesn't contribute", r)) return(1)
            if (grepl("Somewhat contributes", r)) return(2)
            if (grepl("Contributes", r) && !grepl("Somewhat", r) && !grepl("Essential", r)) return(3)
            if (grepl("Very helpful", r)) return(4)
            if (grepl("Essential", r)) return(5)
            return(NA)
          })
          mean(numeric_responses, na.rm = TRUE)
        } else {
          NA
        }
      } else {
        NA
      }
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      method = gsub("^x\\.learning\\.\\.", "", learning_questions),
      avg_score = avg_scores,
      stringsAsFactors = FALSE
    )
    plot_data <- plot_data[!is.na(plot_data$avg_score), ]
    
    # Order by average score and take top 7
    plot_data <- plot_data[order(plot_data$avg_score, decreasing = TRUE), ]
    plot_data <- head(plot_data, 7)
    
    # Create horizontal bar chart
    p <- ggplot(plot_data, aes(x = reorder(method, avg_score), y = avg_score)) +
      geom_col_interactive(
        aes(
          tooltip = paste(method, ": ", round(avg_score, 2), "/ 5.00", sep = ""),
          data_id = method
        ),
        fill = "#e67e22",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Learning Method", y = "Average Rating (1-5)", title = "") +
      coord_flip() +
      ylim(0, 5)
    
    return(list(plot = p, data = plot_data))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate community connection score plot
# Creates a gauge-style visualization showing average community connection score
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_community_connection_plot <- function(df) {
  if (!is.null(df)) {
    # Community question columns - use centralized mapping
    community_questions <- survey_columns$community
    
    # Calculate average scores for each question
    avg_scores <- sapply(community_questions, function(q) {
      if (q %in% names(df)) {
        responses <- df[[q]]
        responses <- responses[!is.na(responses)]
        if (length(responses) > 0) {
          # Convert Likert scale to numeric
          numeric_responses <- sapply(responses, function(r) {
            if (grepl("Not at all", r)) return(1)
            if (grepl("Not very much", r)) return(2)
            if (grepl("Somewhat", r)) return(3)
            if (grepl("Very much", r) && !grepl("Very much so", r)) return(4)
            if (grepl("Very much so", r)) return(5)
            return(NA)
          })
          mean(numeric_responses, na.rm = TRUE)
        } else {
          NA
        }
      } else {
        NA
      }
    })
    
    # Calculate overall average
    overall_avg <- mean(avg_scores, na.rm = TRUE)
    
    # Create data frame for plotting
    plot_data <- data.frame(
      question = gsub("^x\\.community\\.\\.", "", community_questions),
      avg_score = avg_scores,
      stringsAsFactors = FALSE
    )
    plot_data <- plot_data[!is.na(plot_data$avg_score), ]
    
    # Order by average score
    plot_data <- plot_data[order(plot_data$avg_score, decreasing = TRUE), ]
    
    # Create horizontal bar chart
    p <- ggplot(plot_data, aes(x = reorder(question, avg_score), y = avg_score)) +
      geom_col_interactive(
        aes(
          tooltip = paste(question, ": ", round(avg_score, 2), "/ 5.00", sep = ""),
          data_id = question
        ),
        fill = "#1abc9c",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Community Question", y = "Average Score (1-5)", title = "") +
      coord_flip() +
      ylim(0, 5)
    
    return(list(plot = p, data = plot_data, overall_avg = overall_avg))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL, overall_avg = NA))
  }
}

# Generate section comparison plot
# Creates a grouped bar chart comparing key metrics across sections
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_section_comparison_plot <- function(df) {
  if (!is.null(df) && "section" %in% names(df)) {
    # Filter out NA sections
    df_filtered <- df[!is.na(df$section), ]
    
    # Calculate average course satisfaction by section - use centralized mapping
    course_questions <- survey_columns$course
    
    section_satisfaction <- sapply(unique(df_filtered$section), function(sec) {
      sec_data <- df_filtered[df_filtered$section == sec, ]
      scores <- sapply(course_questions, function(q) {
        if (q %in% names(sec_data)) {
          responses <- sec_data[[q]]
          responses <- responses[!is.na(responses)]
          if (length(responses) > 0) {
            numeric_responses <- sapply(responses, function(r) {
              if (grepl("Strongly Disagree", r)) return(1)
              if (grepl("Disagree", r) && !grepl("Strongly", r)) return(2)
              if (grepl("Neutral", r)) return(3)
              if (grepl("Agree", r) && !grepl("Strongly", r)) return(4)
              if (grepl("Strongly Agree", r)) return(5)
              return(NA)
            })
            mean(numeric_responses, na.rm = TRUE)
          } else {
            NA
          }
        } else {
          NA
        }
      })
      mean(scores, na.rm = TRUE)
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      section = names(section_satisfaction),
      avg_satisfaction = as.numeric(section_satisfaction),
      stringsAsFactors = FALSE
    )
    plot_data <- plot_data[!is.na(plot_data$avg_satisfaction), ]
    
    # Create bar chart
    p <- ggplot(plot_data, aes(x = section, y = avg_satisfaction)) +
      geom_col_interactive(
        aes(
          tooltip = paste("Section ", section, ": ", round(avg_satisfaction, 2), "/ 5.00", sep = ""),
          data_id = section
        ),
        fill = "#34495e",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Section", y = "Avg. Satisfaction Score (1-5)", title = "") +
      ylim(0, 5)
    
    return(list(plot = p, data = plot_data))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate key themes from free text plot
# Creates a word cloud or bar chart showing common themes from free text responses
#
# Parameters:
#   df: Data frame containing survey responses
#
# Returns:
#   List with:
#     - plot: ggplot object with interactive elements for ggiraph rendering
#     - data: Data frame used for plotting
generate_key_themes_plot <- function(df) {
  if (!is.null(df)) {
    # Free text columns to analyze
    free_text_cols <- c(
      "free_text_challenge_meeting_people",
      "free_text_class_welcoming_inclusive",
      "free_text_class_interesting_engaging",
      "free_text_learning_meeting_expectations",
      "free_text_hopes_interacting_students",
      "free_text_hopes_interacting_professor",
      "free_text_class_favorite_part",
      "free_text_class_least_enjoyable_part",
      "free_text_more_and_less_of"
    )
    
    # Combine all free text responses
    all_text <- unlist(lapply(free_text_cols, function(col) {
      if (col %in% names(df)) {
        responses <- df[[col]]
        responses <- responses[!is.na(responses) & responses != ""]
        paste(responses, collapse = " ")
      } else {
        ""
      }
    }))
    
    # Simple word frequency analysis
    words <- unlist(strsplit(tolower(all_text), "\\s+"))
    words <- words[nchar(words) > 3]  # Filter short words
    words <- words[!words %in% c("the", "and", "that", "this", "with", "have", "from", "they", "been", "were", "what", "when", "like", "just", "more", "very", "really", "feel", "think", "know", "time", "people", "class", "course", "learning", "discord", "professor", "students", "online", "in-person")]
    
    word_counts <- sort(table(words), decreasing = TRUE)
    top_words <- head(word_counts, 10)
    
    # Create data frame for plotting
    plot_data <- data.frame(
      word = names(top_words),
      count = as.numeric(top_words),
      stringsAsFactors = FALSE
    )
    
    # Create horizontal bar chart
    p <- ggplot(plot_data, aes(x = reorder(word, count), y = count)) +
      geom_col_interactive(
        aes(
          tooltip = paste(word, ": ", count, " mentions", sep = ""),
          data_id = word
        ),
        fill = "#e74c3c",
        width = 0.7
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Word/Theme", y = "Frequency", title = "") +
      coord_flip()
    
    return(list(plot = p, data = plot_data))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}