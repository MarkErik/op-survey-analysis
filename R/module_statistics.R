statisticsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(
          class = "category-navigation",
          h3("Statistics Analysis", class = "section-title"),
          div(
            class = "category-selector",
            selectInput(
              ns("category_select"),
              label = "Select Question Category:",
              choices = c(
                "Course Satisfaction" = "course_satisfaction",
                "Learning Methods" = "learning_methods",
                "Community & Belonging" = "community_belonging"
              ),
              selected = "course_satisfaction"
            )
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "question-selector-section",
          div(
            class = "question-count-display",
            span(id = ns("question_count_label"), "6 questions")
          ),
          div(
            class = "question-buttons-row",
            lapply(names(QUESTION_GROUPS$course_satisfaction), function(q_name) {
              btn_id <- ns(paste0("q_", q_name))
              actionButton(
                btn_id,
                label = truncate_text(q_name, 30),
                class = "question-btn",
                icon = icon("question")
              )
            })
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "section-comparison-toggle",
          checkboxInput(
            ns("compare_sections"),
            label = "Compare Across Sections",
            value = FALSE
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "stats-summary-panel",
          h4("Descriptive Statistics", class = "stats-title"),
          DT::dataTableOutput(ns("stats_table"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "histogram-section",
          h4("Response Distribution", class = "histogram-title"),
          plotOutput(ns("likert_histogram"),
            height = "400px",
            tooltip = TRUE
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "section-comparison-panel",
          h4("Section-by-Section Comparison", class = "comparison-title"),
          plotOutput(ns("section_comparison_histogram"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    )
  )
}

statisticsServer <- function(id, data_server = NULL, filter_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    filtered_data <- reactive({
      tryCatch({
        if (!is.null(data_server)) {
          data_server$getDataReactive()
        } else {
          reactive({
            tibble::tibble()
          })()
        }
      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    selected_category <- reactiveVal("course_satisfaction")

    selected_question <- reactiveVal(NULL)

    question_count_label <- reactive({
      cat <- selected_category()
      n_questions <- length(QUESTION_GROUPS[[cat]])
      paste0(n_questions, " questions")
    })

    observeEvent(input$category_select, {
      selected_category(input$category_select)
      selected_question(NULL)
    })

    observeEvent(names(QUESTION_GROUPS$course_satisfaction), {
      lapply(names(QUESTION_GROUPS$course_satisfaction), function(q_name) {
        btn_id <- ns(paste0("q_", q_name))
        observeEvent(input[[btn_id]], {
          selected_question(q_name)
        })
      })
    })

    question_stats <- reactive({
      tryCatch({
        data <- filtered_data()
        q_key <- selected_question()

        if (nrow(data) == 0 || is.null(q_key)) {
          return(list(
            n = 0,
            mean = NA,
            median = NA,
            mode = NA,
            sd = NA,
            se = NA,
            min = NA,
            max = NA,
            q1 = NA,
            q3 = NA,
            missing = 0
          ))
        }

        q_col <- QUESTION_GROUPS[[selected_category()]][[q_key]]

        likert_values <- data %>%
          dplyr::select(dplyr::all_of(q_col)) %>%
          dplyr::mutate(
            value = extract_likert_value(dplyr::all_of(q_col))
          ) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::pull(value)

        stats <- calculate_descriptive_stats(likert_values)

        return(stats)

      }, error = function(e) {
        return(list(
          n = 0,
          mean = NA,
          median = NA,
          mode = NA,
          sd = NA,
          se = NA,
          min = NA,
          max = NA,
          q1 = NA,
          q3 = NA,
          missing = 0
        ))
      })
    })

    section_comparison_data <- reactive({
      tryCatch({
        data <- filtered_data()
        q_key <- selected_question()

        if (nrow(data) == 0 || is.null(q_key)) {
          return(tibble::tibble())
        }

        q_col <- QUESTION_GROUPS[[selected_category()]][[q_key]]

        comp_data <- data %>%
          dplyr::select(dplyr::all_of(COL_SECTION), dplyr::all_of(q_col)) %>%
          dplyr::mutate(
            value = extract_likert_value(dplyr::all_of(q_col))
          ) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(dplyr::all_of(COL_SECTION)) %>%
          dplyr::summarise(
            n = dplyr::n(),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            q1 = quantile(value, 0.25, na.rm = TRUE),
            q3 = quantile(value, 0.75, na.rm = TRUE)
          ) %>%
          dplyr::arrange(dplyr::desc(mean))

        return(comp_data)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$stats_table <- DT::renderDataTable({
      stats <- question_stats()

      stats_df <- tibble::tibble(
        Statistic = c("N", "Mean", "Median", "Mode", "SD", "SE", "Min", "Max", "Q1", "Q3", "Missing"),
        Value = c(
          stats$n,
          ifelse(is.na(stats$mean), NA, round(stats$mean, 2)),
          ifelse(is.na(stats$median), NA, round(stats$median, 2)),
          ifelse(is.na(stats$mode), NA, as.character(stats$mode)),
          ifelse(is.na(stats$sd), NA, round(stats$sd, 2)),
          ifelse(is.na(stats$se), NA, round(stats$se, 2)),
          ifelse(is.na(stats$min), NA, stats$min),
          ifelse(is.na(stats$max), NA, stats$max),
          ifelse(is.na(stats$q1), NA, round(stats$q1, 2)),
          ifelse(is.na(stats$q3), NA, round(stats$q3, 2)),
          stats$missing
        )
      )

      DT::datatable(
        stats_df,
        options = list(
          pageLength = 11,
          scrollX = TRUE,
          scrollY = FALSE,
          searching = FALSE,
          ordering = FALSE,
          paging = FALSE,
          info = FALSE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Statistic", "Value")
      )
    })

    output$question_count_label <- renderText({
      question_count_label()
    })

    output$likert_histogram <- renderGirafe({
      data <- filtered_data()
      q_key <- selected_question()

      if (nrow(data) == 0 || is.null(q_key)) {
        girafe(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Select a question to view histogram", size = 5) +
            ggplot2::theme_void(),
          width = "100%",
          height = "100%",
          zoom_min = 0.5,
          zoom_max = 3,
          zoom_ondblclick = FALSE
        )
        return()
      }

      q_col <- QUESTION_GROUPS[[selected_category()]][[q_key]]

      likert_counts <- data %>%
        dplyr::select(dplyr::all_of(q_col)) %>%
        dplyr::mutate(
          value = extract_likert_value(dplyr::all_of(q_col))
        ) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::count(value, sort = TRUE) %>%
        dplyr::mutate(
          percentage = round(n / sum(n) * 100, 1)
        )

      likert_labels <- LIKERT_SCALE

      likert_colors <- LIKERT_COLORS

      p <- ggplot2::ggplot(likert_counts, ggplot2::aes(x = value, y = n, fill = value)) +
        ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Score: ", value, "<br>Count: ", n, "<br>Percentage: ", percentage, "%"))) +
        ggplot2::scale_x_discrete(limits = LIKERT_NUMERIC_VALUES, labels = likert_labels) +
        ggplot2::scale_fill_manual(values = likert_colors) +
        ggplot2::labs(
          title = paste("Response Distribution for:", q_key),
          x = "Likert Scale",
          y = "Number of Responses"
        ) +
        PLOT_THEME +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.minor = ggplot2::element_blank()
        )

      girafe(
        p,
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    output$section_comparison_histogram <- renderGirafe({
      comp_data <- section_comparison_data()

      if (nrow(comp_data) == 0) {
        girafe(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Enable section comparison to view breakdown", size = 5) +
            ggplot2::theme_void(),
          width = "100%",
          height = "100%",
          zoom_min = 0.5,
          zoom_max = 3,
          zoom_ondblclick = FALSE
        )
        return()
      }

      likert_labels <- LIKERT_SCALE

      likert_colors <- LIKERT_COLORS

      p <- ggplot2::ggplot(comp_data, ggplot2::aes(x = value, y = n, fill = value)) +
        ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Section: ", dplyr::all_of(COL_SECTION), "<br>Score: ", value, "<br>Count: ", n, "<br>Mean: ", round(mean, 2)))) +
        ggplot2::facet_wrap(~ dplyr::all_of(COL_SECTION), scales = "free_y") +
        ggplot2::scale_x_discrete(limits = LIKERT_NUMERIC_VALUES, labels = likert_labels) +
        ggplot2::scale_fill_manual(values = likert_colors) +
        ggplot2::labs(
          title = "Section-by-Section Response Distribution",
          x = "Likert Scale",
          y = "Number of Responses"
        ) +
        PLOT_THEME +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.minor = ggplot2::element_blank()
        )

      girafe(
        p,
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    observeEvent(input$compare_sections, {
      if (input$compare_sections) {
        shinyjs::show("section-comparison-panel")
      } else {
        shinyjs::hide("section-comparison-panel")
      }
    })

    return(list(
      selected_category = selected_category,
      selected_question = selected_question,
      question_stats = question_stats,
      section_comparison_data = section_comparison_data
    ))
  })
}
