# Course Content Tab Plot Generation
# Functions for generating Course Content tab visualizations

# Color palette (Anthropic-inspired warm neutrals)
COLORS <- list(
  primary = "#8B7355",
  primary_dark = "#5D4E37",
  primary_light = "#C4A77D",
  secondary = "#6B6B6B",
  secondary_dark = "#4A4A4A",
  secondary_light = "#9E9E9E",
  background = "#FAF8F5",
  surface = "#FFFFFF",
  border = "#E8E4DD",
  text_primary = "#3D3D3D",
  text_secondary = "#6B6B6B",
  text_muted = "#A0A0A0",
  accent = "#D4AF37",
  success = "#7D9A7D",
  warning = "#D4A574",
  error = "#C47D7D",
  info = "#7D9DC4",
  # Likert scale colors
  strongly_disagree = "#C47D7D",
  disagree = "#D49A7D",
  neutral = "#9E9E9E",
  agree = "#9DC47D",
  strongly_agree = "#7DA07D"
)

#' Generate Agreement Statement Plot
#'
#' Creates a diverging stacked bar chart for a single agreement statement.
#' Shows the distribution of responses across the 5-point Likert scale.
#'
#' @param df A data frame containing survey responses
#' @param column The column name for the agreement statement
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_agreement_statement_plot <- function(df, column, title = NULL) {
  if (is.null(df) || !column %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Get display name for the column
  display_name <- if (!is.null(title)) {
    title
  } else {
    get_column_display_name(column)
  }
  
  # Count responses
  plot_data <- df %>%
    filter(!is.na(!!sym(column))) %>%
    count(!!sym(column), name = "count") %>%
    mutate(
      response = as.character(!!sym(column)),
      response_label = case_when(
        response == "1" ~ "Strongly Disagree",
        response == "2" ~ "Disagree",
        response == "3" ~ "Neutral",
        response == "4" ~ "Agree",
        response == "5" ~ "Strongly Agree",
        TRUE ~ response
      ),
      response_label = factor(response_label,
                              levels = c("Strongly Disagree", "Disagree", "Neutral",
                                        "Agree", "Strongly Agree")),
      percentage = round(count / sum(count) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    ) %>%
    arrange(response_label)
  
  # Create diverging stacked bar chart
  p <- ggplot(plot_data, aes(x = "", y = percentage, fill = response_label)) +
    geom_col(width = 0.6, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              size = 3.5, color = COLORS$text_primary) +
    scale_fill_manual(
      values = c("Strongly Disagree" = COLORS$strongly_disagree,
                 "Disagree" = COLORS$disagree,
                 "Neutral" = COLORS$neutral,
                 "Agree" = COLORS$agree,
                 "Strongly Agree" = COLORS$strongly_agree),
      name = "Response"
    ) +
    coord_polar(theta = "y") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      legend.position = "right",
      legend.title = element_text(size = 12, color = COLORS$text_secondary),
      legend.text = element_text(size = 11, color = COLORS$text_primary),
      plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(title = display_name)
  
  return(list(plot = p, data = plot_data))
}

#' Generate Agreement Comparison Plot
#'
#' Creates a grouped comparison chart for agreement statements across sections
#' or experience levels.
#'
#' @param df A data frame containing survey responses
#' @param columns Vector of column names for agreement statements
#' @param group_by Column name to group by (e.g., "section", "prior_experience")
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_agreement_comparison_plot <- function(df, columns, group_by, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Check if columns exist
  available_columns <- columns[columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No agreement data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Check if group_by column exists
  if (!group_by %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Group column not available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Reshape data for plotting
  plot_data <- df %>%
    select(all_of(c("participant_id", group_by, available_columns))) %>%
    pivot_longer(
      cols = all_of(available_columns),
      names_to = "statement",
      values_to = "response"
    ) %>%
    filter(!is.na(response) & !is.na(!!sym(group_by))) %>%
    group_by(statement, !!sym(group_by)) %>%
    summarise(
      avg_score = mean(as.numeric(response), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      statement_short = case_when(
        statement == "how_much_do_you_agree_with_the_statement_1" ~ "Statement 1",
        statement == "how_much_do_you_agree_with_the_statement_2" ~ "Statement 2",
        statement == "how_much_do_you_agree_with_the_statement_3" ~ "Statement 3",
        statement == "how_much_do_you_agree_with_the_statement_4" ~ "Statement 4",
        statement == "how_much_do_you_agree_with_the_statement_5" ~ "Statement 5",
        statement == "how_much_do_you_agree_with_the_statement_6" ~ "Statement 6",
        TRUE ~ statement
      ),
      statement_short = factor(statement_short, levels = paste0("Statement ", 1:6))
    )
  
  # Create grouped bar chart
  p <- ggplot(plot_data, aes(x = statement_short, y = avg_score, fill = !!sym(group_by))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7,
             color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = round(avg_score, 2)),
              position = position_dodge(width = 0.8), vjust = -0.5,
              size = 3, color = COLORS$text_primary) +
    scale_fill_manual(values = c(COLORS$primary, COLORS$primary_light, COLORS$info),
                      name = group_by) +
    scale_y_continuous(limits = c(0, 5.5), breaks = 1:5) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = COLORS$text_primary),
      axis.text.y = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12, color = COLORS$text_secondary, margin = margin(r = 10)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      legend.position = "right",
      legend.title = element_text(size = 11, color = COLORS$text_secondary),
      legend.text = element_text(size = 10, color = COLORS$text_primary),
      plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Average Score (1-5)", title = title)
  
  return(list(plot = p, data = plot_data))
}

#' Generate Learning Preference Plot
#'
#' Creates a visualization showing the distribution of learning preferences.
#' Can be shown as a bar chart or stacked bar chart with comparisons.
#'
#' @param df A data frame containing survey responses
#' @param group_by Optional column name to group by for comparison
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_learning_preference_plot <- function(df, group_by = NULL, title = NULL) {
  if (is.null(df) || !"learning_preference" %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  plot_title <- if (!is.null(title)) title else "Learning Format Preference"
  
  if (is.null(group_by)) {
    # Simple distribution plot
    plot_data <- df %>%
      filter(!is.na(learning_preference)) %>%
      count(learning_preference, name = "count") %>%
      mutate(
        percentage = round(count / sum(count) * 100, 1),
        label = paste0(count, " (", percentage, "%)")
      ) %>%
      arrange(desc(count)) %>%
      mutate(learning_preference = factor(learning_preference, levels = learning_preference))
    
    p <- ggplot(plot_data, aes(x = learning_preference, y = count, fill = learning_preference)) +
      geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
      geom_text(aes(label = label), vjust = -0.5, size = 3.5, color = COLORS$text_primary) +
      scale_fill_manual(
        values = c("In-person" = COLORS$primary,
                   "Online" = COLORS$info,
                   "No preference" = COLORS$accent),
        guide = "none"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(hjust = 0.5, size = 12, color = COLORS$text_primary),
        axis.text.y = element_text(size = 12, color = COLORS$text_primary),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, color = COLORS$text_secondary, margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = COLORS$surface, color = NA),
        plot.background = element_rect(fill = COLORS$surface, color = NA),
        plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      labs(y = "Number of Responses", title = plot_title) +
      coord_cartesian(ylim = c(0, max(plot_data$count) * 1.15))
  } else {
    # Grouped comparison plot
    if (!group_by %in% names(df)) {
      p <- ggplot() +
        geom_blank() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "Group column not available", size = 5)
      return(list(plot = p, data = NULL))
    }
    
    plot_data <- df %>%
      filter(!is.na(learning_preference) & !is.na(!!sym(group_by))) %>%
      count(learning_preference, !!sym(group_by), name = "count") %>%
      group_by(!!sym(group_by)) %>%
      mutate(percentage = round(count / sum(count) * 100, 1)) %>%
      ungroup()
    
    p <- ggplot(plot_data, aes(x = !!sym(group_by), y = percentage, fill = learning_preference)) +
      geom_col(position = "stack", width = 0.7, color = COLORS$border, linewidth = 0.5) +
      scale_fill_manual(
        values = c("In-person" = COLORS$primary,
                   "Online" = COLORS$info,
                   "No preference" = COLORS$accent),
        name = "Preference"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = COLORS$text_primary),
        axis.text.y = element_text(size = 11, color = COLORS$text_primary),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = COLORS$text_secondary, margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = COLORS$surface, color = NA),
        plot.background = element_rect(fill = COLORS$surface, color = NA),
        legend.position = "right",
        legend.title = element_text(size = 11, color = COLORS$text_secondary),
        legend.text = element_text(size = 10, color = COLORS$text_primary),
        plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      labs(y = "Percentage", title = plot_title)
  }
  
  return(list(plot = p, data = plot_data))
}

#' Generate Expectations Wordcloud
#'
#' Creates a word cloud visualization for course expectations.
#' This is an optional visualization that requires the wordcloud package.
#'
#' @param df A data frame containing survey responses
#' @param column The column name containing expectations text
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_expectations_wordcloud <- function(df, column, title = NULL) {
  if (is.null(df) || !column %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Extract text and process for word cloud
  text_data <- df %>%
    filter(!is.na(!!sym(column)) & !!sym(column) != "") %>%
    pull(!!sym(column))
  
  if (length(text_data) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No text data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Combine all text
  all_text <- paste(text_data, collapse = " ")
  
  # Simple word frequency (basic implementation)
  words <- unlist(strsplit(tolower(all_text), "\\s+"))
  words <- words[words != "" & nchar(words) > 2]
  
  # Remove common stop words
  stop_words <- c("the", "and", "for", "are", "but", "not", "you", "all", "can", "had",
                  "her", "was", "one", "our", "out", "has", "have", "been", "will", "with",
                  "this", "that", "from", "they", "would", "there", "their", "what", "about",
                  "which", "when", "make", "like", "just", "over", "such", "into", "than",
                  "more", "very", "some", "them", "than", "then", "now", "only", "also")
  words <- words[!words %in% stop_words]
  
  # Count word frequencies
  word_counts <- as.data.frame(table(words), stringsAsFactors = FALSE)
  colnames(word_counts) <- c("word", "freq")
  word_counts <- word_counts %>%
    arrange(desc(freq)) %>%
    head(50) %>%
    mutate(
      word = factor(word, levels = rev(word)),
      label = paste0(word, " (", freq, ")")
    )
  
  # Create horizontal bar chart as alternative to word cloud
  p <- ggplot(word_counts, aes(x = freq, y = word, fill = freq)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = freq), hjust = -0.2, size = 3, color = COLORS$text_primary) +
    scale_fill_gradient(low = COLORS$primary_light, high = COLORS$primary, guide = "none") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, color = COLORS$text_primary),
      axis.text.y = element_text(size = 10, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(title = if (!is.null(title)) title else "Course Expectations - Top Words") +
    coord_flip() +
    coord_cartesian(xlim = c(0, max(word_counts$freq) * 1.1))
  
  return(list(plot = p, data = word_counts))
}

#' Generate Agreement Heatmap
#'
#' Creates a heatmap showing the distribution of responses across all 6
#' agreement statements.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_agreement_heatmap <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 6 course agreement statement columns
  agreement_columns <- c(
    "how_much_do_you_agree_with_the_statement_1",
    "how_much_do_you_agree_with_the_statement_2",
    "how_much_do_you_agree_with_the_statement_3",
    "how_much_do_you_agree_with_the_statement_4",
    "how_much_do_you_agree_with_the_statement_5",
    "how_much_do_you_agree_with_the_statement_6"
  )
  
  # Check if columns exist
  available_columns <- agreement_columns[agreement_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No agreement data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Reshape data for heatmap
  plot_data <- df %>%
    select(all_of(c("participant_id", available_columns))) %>%
    pivot_longer(
      cols = all_of(available_columns),
      names_to = "statement",
      values_to = "response"
    ) %>%
    filter(!is.na(response)) %>%
    mutate(
      response_label = case_when(
        response == 1 ~ "Strongly Disagree",
        response == 2 ~ "Disagree",
        response == 3 ~ "Neutral",
        response == 4 ~ "Agree",
        response == 5 ~ "Strongly Agree",
        TRUE ~ as.character(response)
      ),
      response_label = factor(response_label,
                              levels = c("Strongly Disagree", "Disagree", "Neutral",
                                        "Agree", "Strongly Agree")),
      statement_short = case_when(
        statement == "how_much_do_you_agree_with_the_statement_1" ~ "Statement 1",
        statement == "how_much_do_you_agree_with_the_statement_2" ~ "Statement 2",
        statement == "how_much_do_you_agree_with_the_statement_3" ~ "Statement 3",
        statement == "how_much_do_you_agree_with_the_statement_4" ~ "Statement 4",
        statement == "how_much_do_you_agree_with_the_statement_5" ~ "Statement 5",
        statement == "how_much_do_you_agree_with_the_statement_6" ~ "Statement 6",
        TRUE ~ statement
      ),
      statement_short = factor(statement_short, levels = paste0("Statement ", 1:6))
    ) %>%
    count(statement_short, response_label) %>%
    group_by(statement_short) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = response_label, y = statement_short, fill = percentage)) +
    geom_tile(color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = paste0(percentage, "%")), size = 3, color = COLORS$text_primary) +
    scale_fill_gradient2(
      low = COLORS$strongly_disagree,
      mid = COLORS$neutral,
      high = COLORS$strongly_agree,
      midpoint = 20,
      name = "Percentage"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = COLORS$text_primary),
      axis.text.y = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      legend.position = "right",
      legend.title = element_text(size = 11, color = COLORS$text_secondary),
      legend.text = element_text(size = 10, color = COLORS$text_primary),
      plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(title = if (!is.null(title)) title else "Agreement Statement Distribution")
  
  return(list(plot = p, data = plot_data))
}

#' Generate Agreement Rankings Plot
#'
#' Creates a horizontal bar chart showing the average score for each
#' agreement statement, ordered by score.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_agreement_rankings_plot <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 6 course agreement statement columns
  agreement_columns <- c(
    "how_much_do_you_agree_with_the_statement_1",
    "how_much_do_you_agree_with_the_statement_2",
    "how_much_do_you_agree_with_the_statement_3",
    "how_much_do_you_agree_with_the_statement_4",
    "how_much_do_you_agree_with_the_statement_5",
    "how_much_do_you_agree_with_the_statement_6"
  )
  
  # Check if columns exist
  available_columns <- agreement_columns[agreement_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No agreement data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Calculate average scores
  plot_data <- df %>%
    select(all_of(c("participant_id", available_columns))) %>%
    pivot_longer(
      cols = all_of(available_columns),
      names_to = "statement",
      values_to = "response"
    ) %>%
    filter(!is.na(response)) %>%
    group_by(statement) %>%
    summarise(
      avg_score = mean(as.numeric(response), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      statement_short = case_when(
        statement == "how_much_do_you_agree_with_the_statement_1" ~ "Statement 1",
        statement == "how_much_do_you_agree_with_the_statement_2" ~ "Statement 2",
        statement == "how_much_do_you_agree_with_the_statement_3" ~ "Statement 3",
        statement == "how_much_do_you_agree_with_the_statement_4" ~ "Statement 4",
        statement == "how_much_do_you_agree_with_the_statement_5" ~ "Statement 5",
        statement == "how_much_do_you_agree_with_the_statement_6" ~ "Statement 6",
        TRUE ~ statement
      ),
      label = paste0(round(avg_score, 2), " (n=", count, ")")
    ) %>%
    arrange(avg_score) %>%
    mutate(statement_short = factor(statement_short, levels = statement_short))
  
  # Create horizontal bar chart
  p <- ggplot(plot_data, aes(x = avg_score, y = statement_short, fill = avg_score)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), hjust = -0.2, size = 3.5, color = COLORS$text_primary) +
    scale_fill_gradient2(
      low = COLORS$strongly_disagree,
      mid = COLORS$neutral,
      high = COLORS$strongly_agree,
      midpoint = 3,
      guide = "none"
    ) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 11, color = COLORS$text_primary),
      axis.text.y = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_text(size = 12, color = COLORS$text_secondary, margin = margin(r = 10)),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(x = "Average Score (1-5)", title = if (!is.null(title)) title else "Statement Rankings") +
    coord_flip()
  
  return(list(plot = p, data = plot_data))
}
