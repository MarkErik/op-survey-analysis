# Home Tab Server Logic
# Contains reactive logic for the Home tab visualizations
#
# @author Course Instructor
# @version 2.0.0

#' Home Tab Server Function
#'
#' Contains all reactive logic for the Home tab
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv Reactive values containing shared state
#' @export
server_home <- function(input, output, session, rv) {

  # Section filter display
  output$home_section_filter_display <- renderUI({
    section <- rv$selected_section

    if (is.null(section) || section == "") {
      return(NULL)
    }

    div(
      class = "section-filter mb-3",
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px;",
      tags$strong("Filtered by: "),
      tags$span(class = "badge bg-primary", section),
      actionButton(
        inputId = "reset_home_filter",
        label = "Reset",
        icon = icon("xmark"),
        class = "btn-sm btn-outline-danger",
        style = "margin-left: 10px;"
      )
    )
  })

  # Reset filter handler
  observeEvent(input$reset_home_filter, {
    rv$selected_section <- NULL
    rv$current_data <- SURVEY_DATA
    updateSelectInput(session, "section_filter", selected = "")
  })

  # Total responses counter
  output$total_responses <- renderText({
    nrow(rv$current_data)
  })

  # Section breakdown chart
  output$section_breakdown_chart <- renderPlot({
    data <- rv$current_data
    section_col <- get_section_col()

    if (!section_col %in% colnames(data)) {
      return(NULL)
    }

    section_counts <- data %>%
      dplyr::count(.data[[section_col]], name = "count") %>%
      dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "")

    if (nrow(section_counts) == 0) {
      return(NULL)
    }

    ggplot2::ggplot(section_counts, ggplot2::aes(
      x = reorder(.data[[section_col]], -count),
      y = count,
      fill = .data[[section_col]]
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5) +
      ggplot2::labs(
        x = "Section",
        y = "Number of Responses",
        title = "Responses by Section"
      ) +
      get_viz_theme() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  })

  # Section click handler
  observeEvent(input$section_click, {
    data <- rv$current_data
    section_col <- get_section_col()

    if (!section_col %in% colnames(data)) {
      return(NULL)
    }

    # Get clicked section
    click <- input$section_click
    section_counts <- data %>%
      dplyr::count(.data[[section_col]], name = "count") %>%
      dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "")

    if (nrow(section_counts) == 0) {
      return(NULL)
    }

    # Find which section was clicked
    x_pos <- click$x
    x_range <- range(seq_along(section_counts[[section_col]]))

    if (!is.null(x_pos) && x_pos >= 1 && x_pos <= length(section_counts[[section_col]])) {
      selected_section <- section_counts[[section_col]][floor(x_pos)]
      rv$selected_section <- selected_section
      rv$current_data <- filter_by_section(SURVEY_DATA, selected_section)
      updateSelectInput(session, "section_filter", selected = selected_section)
    }
  })

  # Learning preference chart
  output$learning_preference_chart <- renderPlot({
    data <- rv$current_data
    pref_col <- get_learning_preference_col()

    if (!pref_col %in% colnames(data)) {
      return(NULL)
    }

    pref_counts <- data %>%
      dplyr::count(.data[[pref_col]], name = "count") %>%
      dplyr::filter(!is.na(.data[[pref_col]]) & .data[[pref_col]] != "")

    if (nrow(pref_counts) == 0) {
      return(NULL)
    }

    colors <- c("In-person" = "#3498db", "Online" = "#e74c3c", "No preference" = "#95a5a6")

    ggplot2::ggplot(pref_counts, ggplot2::aes(
      x = .data[[pref_col]],
      y = count,
      fill = .data[[pref_col]]
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5) +
      ggplot2::labs(
        x = "Learning Preference",
        y = "Number of Students",
        title = paste0("Learning Preference Distribution (", get_section_label(rv$selected_section), ")")
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme(legend.position = "none")
  })

  # Programming experience chart
  output$programming_experience_chart <- renderPlot({
    data <- rv$current_data
    exp_col <- get_programming_experience_col()

    if (!exp_col %in% colnames(data)) {
      return(NULL)
    }

    exp_counts <- data %>%
      dplyr::count(.data[[exp_col]], name = "count") %>%
      dplyr::filter(!is.na(.data[[exp_col]]) & .data[[exp_col]] != "")

    if (nrow(exp_counts) == 0) {
      return(NULL)
    }

    # Shorten labels
    exp_counts$short_label <- dplyr::case_when(
      stringr::str_detect(.data[[exp_col]], "Highly experienced") ~ "Highly Experienced",
      stringr::str_detect(.data[[exp_col]], "Took programming") ~ "Some Experience",
      stringr::str_detect(.data[[exp_col]], "No experience") ~ "No Experience",
      TRUE ~ .data[[exp_col]]
    )

    colors <- c("Highly Experienced" = "#27ae60", "Some Experience" = "#f39c12", "No Experience" = "#e74c3c")

    ggplot2::ggplot(exp_counts, ggplot2::aes(
      x = reorder(short_label, -count),
      y = count,
      fill = short_label
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5) +
      ggplot2::labs(
        x = "Programming Experience",
        y = "Number of Students",
        title = paste0("Prior Programming Experience (", get_section_label(rv$selected_section), ")")
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme(legend.position = "none")
  })

  # Course satisfaction chart
  output$course_satisfaction_chart <- renderPlot({
    data <- rv$current_data
    cols <- get_course_satisfaction_cols()
    labels <- get_course_satisfaction_labels()

    # Filter to existing columns
    existing_cols <- cols[cols %in% colnames(data)]

    if (length(existing_cols) == 0) {
      return(NULL)
    }

    # Calculate means
    means <- sapply(existing_cols, function(col) {
      mean(data[[col]], na.rm = TRUE)
    })

    satisfaction_df <- data.frame(
      question = names(means),
      mean = means,
      label = labels[match(names(means), cols)]
    )

    ggplot2::ggplot(satisfaction_df, ggplot2::aes(
      x = reorder(label, mean),
      y = mean,
      fill = mean
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), hjust = -0.2) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Satisfaction Statement",
        y = "Average Score (1-5)",
        title = paste0("Course Satisfaction Overview (", get_section_label(rv$selected_section), ")")
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_gradient(low = "#e74c3c", high = "#27ae60") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, 5.5)
  })

  # Discord engagement chart
  output$discord_engagement_chart <- renderPlot({
    data <- rv$current_data

    if (!"discord_joined" %in% colnames(data)) {
      return(NULL)
    }

    n_total <- nrow(data)

    if (n_total == 0) {
      return(NULL)
    }

    discord_stats <- data.frame(
      metric = c("Joined Discord", "Active on Discord", "Find Discord Useful"),
      percentage = c(
        mean(data$discord_joined, na.rm = TRUE) * 100,
        mean(data$discord_active, na.rm = TRUE) * 100,
        mean(data$discord_useful, na.rm = TRUE) * 100
      )
    )

    colors <- c("#9b59b6", "#8e44ad", "#7d3c98")

    ggplot2::ggplot(discord_stats, ggplot2::aes(
      x = reorder(metric, -percentage),
      y = percentage,
      fill = metric
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
      ggplot2::labs(
        x = "Discord Metric",
        y = "Percentage of Students",
        title = paste0("Discord Engagement Metrics (", get_section_label(rv$selected_section), ")")
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, 100)
  })

  # Learning methods chart
  output$learning_methods_chart <- renderPlot({
    data <- rv$current_data
    cols <- get_learning_methods_cols()
    labels <- get_learning_methods_labels()

    # Filter to existing columns
    existing_cols <- cols[cols %in% colnames(data)]

    if (length(existing_cols) == 0) {
      return(NULL)
    }

    # Calculate means
    means <- sapply(existing_cols, function(col) {
      mean(data[[col]], na.rm = TRUE)
    })

    methods_df <- data.frame(
      question = names(means),
      mean = means,
      label = labels[match(names(means), cols)]
    ) %>%
      dplyr::arrange(dplyr::desc(mean))

    ggplot2::ggplot(methods_df, ggplot2::aes(
      x = reorder(label, mean),
      y = mean,
      fill = mean
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), hjust = -0.2) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Learning Method",
        y = "Average Rating (1-5)",
        title = paste0("Most Valuable Learning Methods (", get_section_label(rv$selected_section), ")")
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_gradient(low = "#e74c3c", high = "#27ae60") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, 5.5)
  })

  # Community scores chart
  output$community_scores_chart <- renderPlot({
    data <- rv$current_data
    cols <- get_community_belonging_cols()
    labels <- get_community_belonging_labels()

    # Filter to existing columns
    existing_cols <- cols[cols %in% colnames(data)]

    if (length(existing_cols) == 0) {
      return(NULL)
    }

    # Calculate means
    means <- sapply(existing_cols, function(col) {
      mean(data[[col]], na.rm = TRUE)
    })

    community_df <- data.frame(
      question = names(means),
      mean = means,
      label = labels[match(names(means), cols)]
    )

    ggplot2::ggplot(community_df, ggplot2::aes(
      x = reorder(label, mean),
      y = mean,
      fill = mean
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), hjust = -0.2) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Community Statement",
        y = "Average Score (1-5)",
        title = paste0("Community Connection Scores (", get_section_label(rv$selected_section), ")")
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_gradient(low = "#e74c3c", high = "#27ae60") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, 5.5)
  })

  # Sidebar stats
  output$sidebar_total_responses <- renderUI({
    div(
      style = "text-align: center; padding: 10px;",
      h3(nrow(rv$current_data)),
      p("Total Responses", class = "text-muted")
    )
  })

  output$sidebar_section_count <- renderUI({
    data <- rv$current_data
    section_col <- get_section_col()

    if (!section_col %in% colnames(data)) {
      return(NULL)
    }

    n_sections <- data %>%
      dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
      dplyr::pull(section_col) %>%
      unique() %>%
      length()

    div(
      style = "text-align: center; padding: 10px;",
      h3(n_sections),
      p("Sections", class = "text-muted")
    )
  })
}

#' Get section label for display
#'
#' @param section Section identifier or NULL
#' @return Character string for display
#' @export
get_section_label <- function(section) {
  if (is.null(section) || section == "") {
    "All Sections"
  } else {
    section
  }
}
