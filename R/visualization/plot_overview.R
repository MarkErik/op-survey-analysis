# Overview Tab Plot Generation
# Functions for generating overview tab visualizations

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

#' Generate Section Distribution Plot
#'
#' Creates an interactive bar chart showing the distribution of students across sections.
#' Uses warm neutral colors consistent with the application theme.
#'
#' @param df A data frame containing survey responses with section column
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_section_distribution_plot <- function(df) {
  if (is.null(df) || !"section" %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Count students per section
  section_counts <- df %>%
    filter(!is.na(section)) %>%
    count(section, name = "count") %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    )
  
  # Define custom ordering: 217 first (11am, 1pm, 3pm), then 231 (11am, 1pm, 3pm)
  time_order <- c("11am", "1pm", "3pm")
  section_order <- c("217", "231")
  
  section_counts <- section_counts %>%
    mutate(
      course_number = ifelse(grepl(" - ", section), 
                             trimws(gsub(" - .*", "", section)), 
                             NA),
      section_time = ifelse(grepl(" - ", section), 
                            trimws(gsub(".* - ", "", section)), 
                            NA)
    ) %>%
    arrange(
      match(course_number, section_order),
      match(section_time, time_order)
    ) %>%
    mutate(section = factor(section, levels = section))
  
  # Create bar chart with warm neutral colors
  p <- ggplot(section_counts, aes(x = section, y = count, fill = course_number)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), vjust = -0.5, size = 3.5, color = COLORS$text_primary) +
    scale_fill_manual(values = c("217" = COLORS$primary, "231" = COLORS$primary_light),
                      name = "Course") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = COLORS$text_primary),
      axis.text.y = element_text(size = 12, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14, color = COLORS$text_secondary, margin = margin(r = 10)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      legend.position = "right",
      legend.title = element_text(size = 12, color = COLORS$text_secondary),
      legend.text = element_text(size = 11, color = COLORS$text_primary),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Number of Responses") +
    coord_cartesian(ylim = c(0, max(section_counts$count) * 1.15))
  
  return(list(plot = p, data = section_counts))
}

#' Generate Experience Distribution Plot
#'
#' Creates a horizontal bar chart showing the distribution of programming experience levels.
#' Ordered from least to most experienced.
#'
#' @param df A data frame containing survey responses with prior_experience column
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_experience_distribution_plot <- function(df) {
  if (is.null(df) || !"prior_experience" %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Count experience levels
  exp_counts <- df %>%
    filter(!is.na(prior_experience)) %>%
    count(prior_experience, name = "count") %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    )
  
  # Define experience level ordering
  experience_levels <- c(
    "No experience at all",
    "Took programming course before (either in school, or online tutorials)",
    "Highly experienced (comfortable writing own programs)"
  )
  
  exp_counts <- exp_counts %>%
    mutate(
      prior_experience = factor(prior_experience, levels = experience_levels),
      experience_short = case_when(
        prior_experience == experience_levels[1] ~ "No Experience",
        prior_experience == experience_levels[2] ~ "Some Experience",
        prior_experience == experience_levels[3] ~ "Highly Experienced",
        TRUE ~ as.character(prior_experience)
      )
    ) %>%
    arrange(prior_experience)
  
  # Create horizontal bar chart with gradient colors
  p <- ggplot(exp_counts, aes(x = experience_short, y = count, fill = experience_short)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), hjust = -0.2, size = 3.5, color = COLORS$text_primary) +
    scale_fill_manual(
      values = c("No Experience" = COLORS$error,
                 "Some Experience" = COLORS$warning,
                 "Highly Experienced" = COLORS$success),
      guide = "none"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, color = COLORS$text_primary),
      axis.text.y = element_text(size = 12, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14, color = COLORS$text_secondary, margin = margin(r = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Number of Responses") +
    coord_flip() +
    coord_cartesian(xlim = c(0, max(exp_counts$count) * 1.15))
  
  return(list(plot = p, data = exp_counts))
}

#' Generate Learning Preference Plot
#'
#' Creates a bar chart showing the distribution of learning preferences
#' (In-person, Online, No preference).
#'
#' @param df A data frame containing survey responses with learning_preference column
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_learning_preference_plot <- function(df) {
  if (is.null(df) || !"learning_preference" %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Count learning preferences
  pref_counts <- df %>%
    filter(!is.na(learning_preference)) %>%
    count(learning_preference, name = "count") %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    )
  
  # Order by count descending
  pref_counts <- pref_counts %>%
    arrange(desc(count)) %>%
    mutate(learning_preference = factor(learning_preference, levels = learning_preference))
  
  # Create bar chart with distinct colors for each preference
  p <- ggplot(pref_counts, aes(x = learning_preference, y = count, fill = learning_preference)) +
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
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Number of Responses") +
    coord_cartesian(ylim = c(0, max(pref_counts$count) * 1.15))
  
  return(list(plot = p, data = pref_counts))
}

#' Generate Response Timeline Plot
#'
#' Creates a timeline plot showing when survey responses were submitted.
#' Useful for understanding response patterns over time.
#'
#' @param df A data frame containing survey responses with timestamp column
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_response_timeline_plot <- function(df) {
  if (is.null(df) || !"timestamp" %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Parse timestamps and count responses by date
  timeline_data <- df %>%
    filter(!is.na(timestamp)) %>%
    mutate(
      response_date = as.Date(timestamp, format = "%Y/%m/%d %I:%M:%S %p"),
      response_hour = as.numeric(format(as.POSIXct(timestamp, format = "%Y/%m/%d %I:%M:%S %p"), "%H"))
    ) %>%
    filter(!is.na(response_date)) %>%
    count(response_date, name = "count") %>%
    arrange(response_date) %>%
    mutate(
      cumulative = cumsum(count),
      date_label = format(response_date, "%b %d")
    )
  
  if (nrow(timeline_data) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No valid timestamps", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Create timeline plot
  p <- ggplot(timeline_data, aes(x = response_date, y = count)) +
    geom_col(fill = COLORS$primary, width = 0.8, color = COLORS$border, linewidth = 0.5) +
    geom_line(aes(y = cumulative), color = COLORS$accent, linewidth = 1.2, group = 1) +
    geom_point(aes(y = cumulative), color = COLORS$accent, size = 2) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
    scale_y_continuous(
      name = "Daily Responses",
      sec.axis = sec_axis(~ ., name = "Cumulative Responses", labels = scales::comma)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = COLORS$text_primary),
      axis.text.y.left = element_text(size = 11, color = COLORS$text_primary),
      axis.text.y.right = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y.left = element_text(size = 12, color = COLORS$text_secondary, margin = margin(r = 10)),
      axis.title.y.right = element_text(size = 12, color = COLORS$text_secondary, margin = margin(l = 10)),
      panel.grid.major = element_line(color = COLORS$border, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(title = "Response Timeline")
  
  return(list(plot = p, data = timeline_data))
}

#' Generate Course Agreement Overview Plot
#'
#' Creates a diverging stacked bar chart showing the distribution of responses
#' across the 6 course agreement statements.
#'
#' @param df A data frame containing survey responses with Likert columns
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_course_agreement_overview_plot <- function(df) {
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
  
  # Reshape data for plotting
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
                                        "Agree", "Strongly Agree"))
    ) %>%
    count(statement, response_label) %>%
    group_by(statement) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ungroup()
  
  # Create diverging stacked bar chart
  p <- ggplot(plot_data, aes(x = statement, y = percentage, fill = response_label)) +
    geom_col(position = "stack", width = 0.7, color = COLORS$border, linewidth = 0.3) +
    scale_fill_manual(
      values = c("Strongly Disagree" = COLORS$strongly_disagree,
                 "Disagree" = COLORS$disagree,
                 "Neutral" = COLORS$neutral,
                 "Agree" = COLORS$agree,
                 "Strongly Agree" = COLORS$strongly_agree),
      name = "Response"
    ) +
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
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Percentage of Responses") +
    coord_flip()
  
  return(list(plot = p, data = plot_data))
}
