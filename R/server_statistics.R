# Statistics Tab Server Logic
# Contains reactive logic for detailed statistical analysis
#
# @author Course Instructor
# @version 2.0.0

#' Statistics Tab Server Function
#'
#' Contains all reactive logic for the Statistics tab
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv Reactive values containing shared state
#' @export
server_statistics <- function(input, output, session, rv) {

  # Reactive value for selected category
  selected_category <- reactiveVal("course_satisfaction")

  # Reactive value for selected question
  selected_question <- reactiveVal(NULL)

  # Section filter display
  output$statistics_section_filter_display <- renderUI({
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
        inputId = "reset_statistics_filter",
        label = "Reset",
        icon = icon("xmark"),
        class = "btn-sm btn-outline-danger",
        style = "margin-left: 10px;"
      )
    )
  })

  # Reset filter handler
  observeEvent(input$reset_statistics_filter, {
    rv$selected_section <- NULL
    rv$current_data <- SURVEY_DATA
    updateSelectInput(session, "section_filter", selected = "")
  })

  # Category change handler
  observeEvent(input$stats_category, {
    selected_category(input$stats_category)
    selected_question(NULL)
  })

  # Question buttons
  output$stats_question_buttons <- renderUI({
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_idx <- which(questions %in% colnames(rv$current_data))
    questions <- questions[existing_idx]
    labels <- labels[existing_idx]

    if (length(questions) == 0) {
      return(div(class = "alert alert-warning", "No questions available for this category"))
    }

    buttons <- lapply(seq_along(questions), function(i) {
      actionButton(
        inputId = paste0("stats_question_", i),
        label = labels[i],
        class = "btn-outline-primary question-button",
        style = "margin: 3px;"
      )
    })

    div(class = "question-button-container", buttons)
  })

  # Question button observers
  observe({
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_idx <- which(questions %in% colnames(rv$current_data))
    questions <- questions[existing_idx]
    labels <- labels[existing_idx]

    lapply(seq_along(questions), function(i) {
      observeEvent(input[[paste0("stats_question_", i)]], {
        selected_question(questions[i])
      })
    })
  })

  # Statistical summary panel
  output$stats_summary_panel <- renderUI({
    question <- selected_question()

    if (is.null(question) || question == "") {
      return(div(class = "alert alert-info", "Select a question to view its statistics"))
    }

    data <- rv$current_data
    stats <- calculate_likert_summary(data, question)

    ui_stats_summary_table(stats)
  })

  # Mini histogram in summary panel
  output$stats_mini_histogram <- renderPlot({
    question <- selected_question()

    if (is.null(question) || question == "") {
      return(NULL)
    }

    data <- rv$current_data
    values <- data[[question]]
    values <- values[!is.na(values)]

    if (length(values) == 0) {
      return(NULL)
    }

    # Create distribution data
    dist_data <- data.frame(
      value = factor(1:5),
      count = sapply(1:5, function(x) sum(values == x, na.rm = TRUE))
    )

    colors <- get_likert_colors()

    ggplot2::ggplot(dist_data, ggplot2::aes(x = value, y = count, fill = value)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(x = "Score", y = "Count") +
      get_viz_theme() +
      ggplot2::theme(legend.position = "none")
  })

  # Distribution plot
  output$stats_distribution_plot <- renderPlot({
    question <- selected_question()

    if (is.null(question) || question == "") {
      return(NULL)
    }

    data <- rv$current_data
    values <- data[[question]]
    values <- values[!is.na(values)]

    if (length(values) == 0) {
      return(NULL)
    }

    # Create distribution data with percentages
    dist_data <- data.frame(
      value = factor(1:5, levels = 1:5),
      count = sapply(1:5, function(x) sum(values == x, na.rm = TRUE))
    )
    dist_data$percentage <- dist_data$count / sum(dist_data$count) * 100

    labels <- get_likert_labels()
    colors <- get_likert_colors()

    ggplot2::ggplot(dist_data, ggplot2::aes(x = value, y = count, fill = value)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(count, "\n(", sprintf("%.1f", percentage), "%)")),
        vjust = -0.3,
        size = 3
      ) +
      ggplot2::scale_x_discrete(
        labels = labels,
        breaks = 1:5
      ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        x = "Response",
        y = "Number of Responses",
        title = "Response Distribution"
      ) +
      get_viz_theme() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, max(dist_data$count) * 1.3)
  })

  # Section comparison toggle
  observeEvent(input$stats_section_comparison, {
    # Toggle handled by UI
  })

  # Section comparison panel
  output$stats_section_comparison_panel <- renderUI({
    question <- selected_question()
    show_comparison <- input$stats_section_comparison

    if (is.null(show_comparison) || !show_comparison) {
      return(NULL)
    }

    if (is.null(question) || question == "") {
      return(div(class = "alert alert-info", "Select a question first"))
    }

    ui_section_comparison_panel(question)
  })

  # Section comparison plot
  output$stats_section_comparison_plot <- renderPlot({
    question <- selected_question()
    show_comparison <- input$stats_section_comparison

    if (is.null(show_comparison) || !show_comparison) {
      return(NULL)
    }

    if (is.null(question) || question == "") {
      return(NULL)
    }

    data <- rv$current_data
    section_col <- get_section_col()

    if (!section_col %in% colnames(data)) {
      return(NULL)
    }

    # Group by section and calculate means
    section_means <- data %>%
      dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
      dplyr::group_by(.data[[section_col]]) %>%
      dplyr::summarize(
        mean = mean(.data[[question]], na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )

    if (nrow(section_means) == 0) {
      return(NULL)
    }

    colors <- RColorBrewer::brewer.pal(nrow(section_means), "Set2")

    ggplot2::ggplot(section_means, ggplot2::aes(
      x = reorder(.data[[section_col]], -mean),
      y = mean,
      fill = .data[[section_col]]
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), vjust = -0.3) +
      ggplot2::labs(
        x = "Section",
        y = "Mean Score",
        title = "Comparison by Section"
      ) +
      get_viz_theme() +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, 5.5)
  })

  # Section statistics table
  output$stats_section_stats_table <- DT::renderDataTable({
    question <- selected_question()
    show_comparison <- input$stats_section_comparison

    if (is.null(show_comparison) || !show_comparison) {
      return(NULL)
    }

    if (is.null(question) || question == "") {
      return(NULL)
    }

    data <- rv$current_data
    section_col <- get_section_col()

    if (!section_col %in% colnames(data)) {
      return(NULL)
    }

    # Calculate statistics by section
    section_stats <- data %>%
      dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
      dplyr::group_by(.data[[section_col]]) %>%
      dplyr::summarize(
        N = sum(!is.na(.data[[question]])),
        Mean = mean(.data[[question]], na.rm = TRUE),
        SD = sd(.data[[question]], na.rm = TRUE),
        Median = median(.data[[question]], na.rm = TRUE),
        .groups = "drop"
      )

    DT::datatable(
      section_stats,
      options = list(
        pageLength = 10,
        searching = FALSE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("Mean", "SD", "Median"), digits = 2)
  })
}

#' Calculate distribution statistics
#'
#' Computes distribution-related statistics for a question
#'
#' @param values Numeric vector of response values
#' @return Data frame with distribution statistics
#' @export
calculate_distribution_stats <- function(values) {
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(data.frame())
  }

  data.frame(
    value = 1:5,
    count = sapply(1:5, function(x) sum(values == x, na.rm = TRUE)),
    percentage = sapply(1:5, function(x) sum(values == x, na.rm = TRUE) / length(values) * 100)
  )
}

#' Calculate section-wise statistics
#'
#' Computes statistics for each section
#'
#' @param data Survey data frame
#' @param question_col Question column name
#' @return Data frame with section statistics
#' @export
calculate_section_stats <- function(data, question_col) {
  section_col <- get_section_col()

  if (!section_col %in% colnames(data)) {
    return(data.frame())
  }

  data %>%
    dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
    dplyr::group_by(.data[[section_col]]) %>%
    dplyr::summarize(
      N = sum(!is.na(.data[[question_col]])),
      Mean = mean(.data[[question_col]], na.rm = TRUE),
      SD = sd(.data[[question_col]], na.rm = TRUE),
      Median = median(.data[[question_col]], na.rm = TRUE),
      Min = min(.data[[question_col]], na.rm = TRUE),
      Max = max(.data[[question_col]], na.rm = TRUE),
      .groups = "drop"
    )
}
