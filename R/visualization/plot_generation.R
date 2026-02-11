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

# Generate learning preference distribution plot
# Creates a bar chart showing distribution of learning preferences
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
    
    # Order by count descending
    pref_df <- pref_df[order(pref_df$count, decreasing = TRUE), ]
    
    # Create bar chart with 3 distinct colors
    p <- ggplot(pref_df, aes(x = preference, y = count, fill = preference)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.75) +
      scale_fill_manual(values = c("In-person" = "#f8b4b4", "Online" = "#a8e6cf", "No preference" = "#ffd3b6")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(y = "Number of Responses")
    
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
      geom_col(fill = "#2ecc71", width = 0.7) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Count", title = "") +
      coord_flip() +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.75)

    
    return(list(plot = p, data = exp_df))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL))
  }
}

# Generate section breakdown bar chart
# Creates a bar chart showing the distribution of students across sections
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
    
    # Define custom ordering: 217 first (11am, 1pm, 3pm), then 231 (11am, 1pm, 3pm)
    time_order <- c("11am", "1pm", "3pm")
    section_order <- c("217", "231")
    
    # Extract section number and time for ordering
    section_df$section_num <- gsub("^(\\d+).*", "\\1", section_df$section)
    section_df$time <- gsub("^\\d+\\s*-\\s*", "", section_df$section)
    
    # Create factor levels for custom ordering
    section_df$section <- factor(section_df$section,
                                  levels = paste0(rep(section_order, each = length(time_order)),
                                                  " - ", time_order))
    
    # Sort by the factor levels
    section_df <- section_df[order(section_df$section), ]
    
    # Create bar chart with different shades for 217 and 231
    p <- ggplot(section_df, aes(x = section, y = count, fill = section_num)) +
      geom_col_interactive(
        aes(
          tooltip = paste(section, ": ", count, " responses", sep = ""),
          data_id = section
        ),
        width = 0.7
      ) +
      scale_fill_manual(values = c("217" = "#3498db", "231" = "#5dade2")) +
      scale_x_discrete(drop = FALSE) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(hjust = 0.5, size = 15),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
      ) +
      labs(y = "Number of Responses") +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.75)
    
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
         # Convert Likert scale to numeric using centralized function
         numeric_responses <- convert_to_numeric(responses)
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
      geom_col(fill = "#9b59b6", width = 0.7) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Average Score (1-5)", title = "") +
      coord_flip() +
      ylim(0, 5) +
      geom_hline(yintercept = 0, color = "grey80", linewidth = 0.75)

    
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
      geom_col(fill = "#7289da", width = 0.7) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Percentage (%)", title = "") +
      coord_flip() +
      ylim(0, 100) +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.75)

    
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
          # Convert scale to numeric using centralized function
          numeric_responses <- convert_to_numeric(responses)
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
      geom_col(fill = "#e67e22", width = 0.7) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Average Rating (1-5)", title = "") +
      coord_flip() +
      ylim(0, 5) +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.75)

    
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
         # Convert Likert scale to numeric using centralized function
         numeric_responses <- convert_to_numeric(responses)
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
      geom_col(fill = "#1abc9c", width = 0.7) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Average Score (1-5)", title = "") +
      coord_flip() +
      ylim(0, 5) +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.75)

    
    return(list(plot = p, data = plot_data, overall_avg = overall_avg))
  } else {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    return(list(plot = p, data = NULL, overall_avg = NA))
  }
}
