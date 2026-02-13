# Community & Belonging Tab Plot Generation
# Functions for generating Community & Belonging tab visualizations

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

#' Generate Belonging Statement Plot
#'
#' Creates a diverging stacked bar chart for a single belonging statement.
#' Shows the distribution of responses across the 5-point Likert scale.
#'
#' @param df A data frame containing survey responses
#' @param column The column name for the belonging statement
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_belonging_statement_plot <- function(df, column, title = NULL) {
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

#' Generate Belonging Comparison Plot
#'
#' Creates a grouped comparison chart for belonging statements across sections
#' or experience levels.
#'
#' @param df A data frame containing survey responses
#' @param columns Vector of column names for belonging statements
#' @param group_by Column name to group by (e.g., "section", "prior_experience")
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_belonging_comparison_plot <- function(df, columns, group_by, title = NULL) {
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
      annotate("text", x = 0.5, y = 0.5, label = "No belonging data available", size = 5)
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
      statement_short = sapply(statement, get_display_short),
      statement_short = factor(statement_short, levels = unique(statement_short))
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

#' Generate Discord Usage Plot
#'
#' Creates a horizontal bar chart showing Discord feature usage.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_discord_usage_plot <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Get Discord columns
  discord_cols <- grep("^discord_", names(df), value = TRUE)
  discord_cols <- setdiff(discord_cols, "discord_custom_response")
  
  if (length(discord_cols) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No Discord data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Count usage for each feature
  plot_data <- df %>%
    select(all_of(discord_cols)) %>%
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "feature",
      values_to = "count"
    ) %>%
    mutate(
      feature = gsub("^discord_", "", feature),
      feature = gsub("_", " ", feature),
      feature = tools::toTitleCase(feature),
      total_responses = nrow(df),
      percentage = round(count / total_responses * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    ) %>%
    arrange(desc(count)) %>%
    mutate(feature = factor(feature, levels = feature))
  
  # Create horizontal bar chart
  p <- ggplot(plot_data, aes(x = count, y = feature, fill = count)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), hjust = -0.2, size = 3.5, color = COLORS$text_primary) +
    scale_fill_gradient(low = COLORS$primary_light, high = COLORS$primary, guide = "none") +
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
    labs(x = "Number of Users", title = if (!is.null(title)) title else "Discord Feature Usage") +
    coord_flip() +
    coord_cartesian(xlim = c(0, max(plot_data$count) * 1.15))
  
  return(list(plot = p, data = plot_data))
}

#' Generate Discord Pattern Plot
#'
#' Creates a visualization showing Discord usage patterns across sections.
#'
#' @param df A data frame containing survey responses
#' @param group_by Column name to group by (e.g., "section")
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_discord_pattern_plot <- function(df, group_by = "section", title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
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
  
  # Get Discord columns
  discord_cols <- grep("^discord_", names(df), value = TRUE)
  discord_cols <- setdiff(discord_cols, "discord_custom_response")
  
  if (length(discord_cols) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No Discord data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Calculate usage by group
  plot_data <- df %>%
    select(all_of(c(group_by, discord_cols))) %>%
    filter(!is.na(!!sym(group_by))) %>%
    group_by(!!sym(group_by)) %>%
    summarise(across(all_of(discord_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(
      cols = -all_of(group_by),
      names_to = "feature",
      values_to = "count"
    ) %>%
    mutate(
      feature = gsub("^discord_", "", feature),
      feature = gsub("_", " ", feature),
      feature = tools::toTitleCase(feature)
    )
  
  # Create grouped bar chart
  p <- ggplot(plot_data, aes(x = feature, y = count, fill = !!sym(group_by))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7,
             color = COLORS$border, linewidth = 0.5) +
    scale_fill_manual(values = c(COLORS$primary, COLORS$primary_light, COLORS$info),
                      name = group_by) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = COLORS$text_primary),
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
    labs(y = "Number of Users", title = if (!is.null(title)) title else "Discord Usage by Section")
  
  return(list(plot = p, data = plot_data))
}

#' Generate Belonging Statements Distribution Plot
#'
#' Creates a heatmap showing the distribution of responses across all 5
#' belonging statements.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_belonging_statements_distribution_plot <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 5 belonging statement columns
  belonging_columns <- c(
    "how_much_do_you_agree_with_the_following_statements_1",
    "how_much_do_you_agree_with_the_following_statements_2",
    "how_much_do_you_agree_with_the_following_statements_3",
    "how_much_do_you_agree_with_the_following_statements_4",
    "how_much_do_you_agree_with_the_following_statements_5"
  )
  
  # Check if columns exist
  available_columns <- belonging_columns[belonging_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No belonging data available", size = 5)
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
      statement_short = sapply(statement, get_display_short),
      statement_short = factor(statement_short, levels = unique(statement_short))
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
    labs(title = if (!is.null(title)) title else "Belonging Statements Distribution")
  
  return(list(plot = p, data = plot_data))
}

#' Generate Belonging Score Gauge
#'
#' Creates a gauge/meter chart showing the overall belonging score.
#'
#' @param df A data frame containing survey responses
#' @param title Optional title for the plot
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_belonging_score_gauge <- function(df, title = NULL) {
  if (is.null(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Define the 5 belonging statement columns
  belonging_columns <- c(
    "how_much_do_you_agree_with_the_following_statements_1",
    "how_much_do_you_agree_with_the_following_statements_2",
    "how_much_do_you_agree_with_the_following_statements_3",
    "how_much_do_you_agree_with_the_following_statements_4",
    "how_much_do_you_agree_with_the_following_statements_5"
  )
  
  # Check if columns exist
  available_columns <- belonging_columns[belonging_columns %in% names(df)]
  
  if (length(available_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No belonging data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Calculate overall belonging score
  score_data <- df %>%
    select(all_of(available_columns)) %>%
    mutate(across(everything(), as.numeric)) %>%
    rowwise() %>%
    mutate(
      avg_score = mean(c_across(everything()), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarise(
      overall_avg = mean(avg_score, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      score_percent = (overall_avg - 1) / 4 * 100,
      score_label = round(overall_avg, 2)
    )
  
  # Create gauge chart using a bar chart approach
  gauge_data <- data.frame(
    category = c("Score", "Remaining"),
    value = c(score_data$score_percent, 100 - score_data$score_percent)
  )
  
  p <- ggplot(gauge_data, aes(x = category, y = value, fill = category)) +
    geom_col(width = 0.5, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = ifelse(category == "Score", 
                                 paste0(score_data$score_label, " / 5.0"), "")),
              vjust = -0.5, size = 6, color = COLORS$text_primary, fontface = "bold") +
    scale_fill_manual(
      values = c("Score" = COLORS$success, "Remaining" = COLORS$neutral),
      guide = "none"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12, color = COLORS$text_secondary, margin = margin(r = 10)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.title = element_text(size = 14, color = COLORS$text_primary, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Percentage", title = if (!is.null(title)) title else "Overall Belonging Score")
  
  return(list(plot = p, data = score_data))
}
