# Learning Elements Tab Plot Generation
# Functions for generating Learning Elements tab visualizations

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

#' Generate Learning Elements Ranking Plot
#'
#' Creates a horizontal bar chart showing the average contribution rating
#' for each of the 11 learning elements, ordered by score.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_learning_elements_ranking_plot <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 11 learning element columns
  learning_columns <- c(
    "how_much_do_the_following_elements_contribute_to_your_learning_1",
    "how_much_do_the_following_elements_contribute_to_your_learning_2",
    "how_much_do_the_following_elements_contribute_to_your_learning_3",
    "how_much_do_the_following_elements_contribute_to_your_learning_4",
    "how_much_do_the_following_elements_contribute_to_your_learning_5",
    "how_much_do_the_following_elements_contribute_to_your_learning_6",
    "how_much_do_the_following_elements_contribute_to_your_learning_7",
    "how_much_do_the_following_elements_contribute_to_your_learning_8",
    "how_much_do_the_following_elements_contribute_to_your_learning_9",
    "how_much_do_the_following_elements_contribute_to_your_learning_10",
    "how_much_do_the_following_elements_contribute_to_your_learning_11"
  )
  
  # Check if columns exist
  available_columns <- learning_columns[learning_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No learning elements data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Calculate average scores
  plot_data <- df %>%
    select(all_of(c("participant_id", available_columns))) %>%
    pivot_longer(
      cols = all_of(available_columns),
      names_to = "element",
      values_to = "response"
    ) %>%
    filter(!is.na(response)) %>%
    group_by(element) %>%
    summarise(
      avg_score = mean(as.numeric(response), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      element_short = sapply(element, get_display_short),
      label = paste0(round(avg_score, 2), " (n=", count, ")")
    ) %>%
    arrange(avg_score) %>%
    mutate(element_short = factor(element_short, levels = element_short))
  
  # Create horizontal bar chart
  p <- ggplot(plot_data, aes(x = avg_score, y = element_short, fill = avg_score)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), hjust = -0.2, size = 3.5, color = COLORS$text_primary) +
    scale_fill_gradient2(
      low = COLORS$primary_light,
      mid = COLORS$primary,
      high = COLORS$primary_dark,
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
    labs(x = "Average Contribution Score (1-5)", title = if (!is.null(title)) title else "Learning Elements Rankings") +
    coord_flip()
  
  return(list(plot = p, data = plot_data))
}

#' Generate Elements Comparison Plot
#'
#' Creates a grouped comparison chart for learning elements across sections
#' or experience levels.
#'
#' @param df A data frame containing survey responses
#' @param group_by Column name to group by (e.g., "section", "prior_experience")
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_elements_comparison_plot <- function(df, group_by, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 11 learning element columns
  learning_columns <- c(
    "how_much_do_the_following_elements_contribute_to_your_learning_1",
    "how_much_do_the_following_elements_contribute_to_your_learning_2",
    "how_much_do_the_following_elements_contribute_to_your_learning_3",
    "how_much_do_the_following_elements_contribute_to_your_learning_4",
    "how_much_do_the_following_elements_contribute_to_your_learning_5",
    "how_much_do_the_following_elements_contribute_to_your_learning_6",
    "how_much_do_the_following_elements_contribute_to_your_learning_7",
    "how_much_do_the_following_elements_contribute_to_your_learning_8",
    "how_much_do_the_following_elements_contribute_to_your_learning_9",
    "how_much_do_the_following_elements_contribute_to_your_learning_10",
    "how_much_do_the_following_elements_contribute_to_your_learning_11"
  )
  
  # Check if columns exist
  available_columns <- learning_columns[learning_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No learning elements data available", size = 5)
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
      names_to = "element",
      values_to = "response"
    ) %>%
    filter(!is.na(response) & !is.na(!!sym(group_by))) %>%
    group_by(element, !!sym(group_by)) %>%
    summarise(
      avg_score = mean(as.numeric(response), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      element_short = sapply(element, get_display_short),
      element_short = factor(element_short, levels = unique(element_short))
    )
  
  # Create grouped bar chart
  p <- ggplot(plot_data, aes(x = element_short, y = avg_score, fill = !!sym(group_by))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7,
             color = COLORS$border, linewidth = 0.5) +
    scale_fill_manual(values = c(COLORS$primary, COLORS$primary_light, COLORS$info),
                      name = group_by) +
    scale_y_continuous(limits = c(0, 5.5), breaks = 1:5) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = COLORS$text_primary),
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

#' Generate Elements Correlation Heatmap
#'
#' Creates a correlation heatmap showing relationships between the 11 learning elements.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_elements_correlation_heatmap <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 11 learning element columns
  learning_columns <- c(
    "how_much_do_the_following_elements_contribute_to_your_learning_1",
    "how_much_do_the_following_elements_contribute_to_your_learning_2",
    "how_much_do_the_following_elements_contribute_to_your_learning_3",
    "how_much_do_the_following_elements_contribute_to_your_learning_4",
    "how_much_do_the_following_elements_contribute_to_your_learning_5",
    "how_much_do_the_following_elements_contribute_to_your_learning_6",
    "how_much_do_the_following_elements_contribute_to_your_learning_7",
    "how_much_do_the_following_elements_contribute_to_your_learning_8",
    "how_much_do_the_following_elements_contribute_to_your_learning_9",
    "how_much_do_the_following_elements_contribute_to_your_learning_10",
    "how_much_do_the_following_elements_contribute_to_your_learning_11"
  )
  
  # Check if columns exist
  available_columns <- learning_columns[learning_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No learning elements data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Calculate correlation matrix
  cor_data <- df %>%
    select(all_of(available_columns)) %>%
    mutate(across(everything(), as.numeric)) %>%
    cor(use = "pairwise.complete.obs", method = "spearman")
  
  # Create short labels
  element_labels <- c(
    "Element 1", "Element 2", "Element 3", "Element 4", "Element 5",
    "Element 6", "Element 7", "Element 8", "Element 9", "Element 10", "Element 11"
  )
  
  # Keep only available columns
  available_labels <- element_labels[learning_columns %in% names(df)]
  
  # Reshape for plotting
  plot_data <- as.data.frame(cor_data) %>%
    rownames_to_column(var = "element1") %>%
    pivot_longer(
      cols = -element1,
      names_to = "element2",
      values_to = "correlation"
    ) %>%
    mutate(
      element1 = factor(element1, levels = available_labels),
      element2 = factor(element2, levels = rev(available_labels))
    )
  
  # Create correlation heatmap
  p <- ggplot(plot_data, aes(x = element2, y = element1, fill = correlation)) +
    geom_tile(color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = round(correlation, 2)), size = 2.5, color = COLORS$text_primary) +
    scale_fill_gradient2(
      low = COLORS$error,
      mid = COLORS$neutral,
      high = COLORS$success,
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = COLORS$text_primary),
      axis.text.y = element_text(size = 9, color = COLORS$text_primary),
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
    labs(title = if (!is.null(title)) title else "Learning Elements Correlation Matrix")
  
  return(list(plot = p, data = plot_data))
}

#' Generate Element Distribution Plot
#'
#' Creates a diverging stacked bar chart showing the distribution of responses
#' for each learning element.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_element_distribution_plot <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 11 learning element columns
  learning_columns <- c(
    "how_much_do_the_following_elements_contribute_to_your_learning_1",
    "how_much_do_the_following_elements_contribute_to_your_learning_2",
    "how_much_do_the_following_elements_contribute_to_your_learning_3",
    "how_much_do_the_following_elements_contribute_to_your_learning_4",
    "how_much_do_the_following_elements_contribute_to_your_learning_5",
    "how_much_do_the_following_elements_contribute_to_your_learning_6",
    "how_much_do_the_following_elements_contribute_to_your_learning_7",
    "how_much_do_the_following_elements_contribute_to_your_learning_8",
    "how_much_do_the_following_elements_contribute_to_your_learning_9",
    "how_much_do_the_following_elements_contribute_to_your_learning_10",
    "how_much_do_the_following_elements_contribute_to_your_learning_11"
  )
  
  # Check if columns exist
  available_columns <- learning_columns[learning_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No learning elements data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Reshape data for plotting
  plot_data <- df %>%
    select(all_of(c("participant_id", available_columns))) %>%
    pivot_longer(
      cols = all_of(available_columns),
      names_to = "element",
      values_to = "response"
    ) %>%
    filter(!is.na(response)) %>%
    mutate(
      response_label = case_when(
        response == 1 ~ "Not at all",
        response == 2 ~ "Slightly",
        response == 3 ~ "Moderately",
        response == 4 ~ "Significantly",
        response == 5 ~ "Very significantly",
        TRUE ~ as.character(response)
      ),
      response_label = factor(response_label,
                              levels = c("Not at all", "Slightly", "Moderately",
                                        "Significantly", "Very significantly")),
      element_short = sapply(element, get_display_short),
      element_short = factor(element_short, levels = unique(element_short))
    ) %>%
    count(element_short, response_label) %>%
    group_by(element_short) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Create diverging stacked bar chart
  p <- ggplot(plot_data, aes(x = element_short, y = percentage, fill = response_label)) +
    geom_col(position = "stack", width = 0.7, color = COLORS$border, linewidth = 0.3) +
    scale_fill_manual(
      values = c("Not at all" = COLORS$strongly_disagree,
                 "Slightly" = COLORS$disagree,
                 "Moderately" = COLORS$neutral,
                 "Significantly" = COLORS$agree,
                 "Very significantly" = COLORS$strongly_agree),
      name = "Contribution"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = COLORS$text_primary),
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
    labs(y = "Percentage of Responses", title = if (!is.null(title)) title else "Element Contribution Distribution") +
    coord_flip()
  
  return(list(plot = p, data = plot_data))
}

#' Generate Single Element Plot
#'
#' Creates a diverging stacked bar chart for a single learning element.
#'
#' @param df A data frame containing survey responses
#' @param column The column name for the learning element
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_single_element_plot <- function(df, column, title = NULL) {
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
        response == "1" ~ "Not at all",
        response == "2" ~ "Slightly",
        response == "3" ~ "Moderately",
        response == "4" ~ "Significantly",
        response == "5" ~ "Very significantly",
        TRUE ~ response
      ),
      response_label = factor(response_label,
                              levels = c("Not at all", "Slightly", "Moderately",
                                        "Significantly", "Very significantly")),
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
      values = c("Not at all" = COLORS$strongly_disagree,
                 "Slightly" = COLORS$disagree,
                 "Moderately" = COLORS$neutral,
                 "Significantly" = COLORS$agree,
                 "Very significantly" = COLORS$strongly_agree),
      name = "Contribution"
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
