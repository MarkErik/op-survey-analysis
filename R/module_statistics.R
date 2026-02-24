# R/module_statistics.R
# Statistics tab module for the CPSC Experience Survey Explorer
# Provides UI and server functions for detailed statistical analysis of Likert-scale questions

# =============================================================================
# UI Module - Statistics Tab
# =============================================================================

#' Statistics UI module function
#'
#' Creates the Statistics tab UI with category navigation, question selector,
#' statistical summary panel, histogram visualization, and section comparison.
#'
#' @param id Character module ID for namespacing
#' @return UI element for the Statistics tab
#' @export
statisticsUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Category Navigation Section
    fluidRow(
      column(12,
        div(
          class = "category-navigation",
          h3("Statistics Analysis", class = "section-title"),
          # Category selector dropdown
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

    # Question Selector Section
    fluidRow(
      column(12,
        div(
          class = "question-selector-section",
          # Question count display
          div(
            class = "question-count-display",
            span(id = ns("question_count_label"), "6 questions")
          ),
          # Horizontal row of clickable buttons for each question
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

    # Section Comparison Toggle
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

    # Statistical Summary Panel
    fluidRow(
      column(12,
        div(
          class = "stats-summary-panel",
          h4("Descriptive Statistics", class = "stats-title"),
          # Stats table
          DT::dataTableOutput(ns("stats_table"))
        )
      )
    ),

    # Histogram Visualization
    fluidRow(
      column(12,
        div(
          class = "histogram-section",
          h4("Response Distribution", class = "histogram-title"),
          # Histogram plot
          plotOutput(ns("likert_histogram"),
            height = "400px",
            tooltip = TRUE
          )
        )
      )
    ),

    # Section Comparison Panel (hidden by default)
    fluidRow(
      column(12,
        div(
          class = "section-comparison-panel",
          h4("Section-by-Section Comparison", class = "comparison-title"),
          # Section comparison plots
          plotOutput(ns("section_comparison_histogram"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    )
  )
}

# =============================================================================
# Server Module - Statistics Tab
# =============================================================================

#' Statistics module server function
#'
#' Provides reactive data expressions, statistics calculations, and histogram
#' rendering for the Statistics tab.
#'
#' @param id Character module ID for namespacing
#' @param data_server Reactive data server module (optional)
#' @param filter_server Reactive filter server module (optional)
#' @return List of reactive expressions and outputs for the Statistics tab
#' @export
statisticsServer <- function(id, data_server = NULL, filter_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # =============================================================================
    # Reactive Data Access
    # =============================================================================

    #' Reactive expression for filtered data
    filtered_data <- reactive({
      tryCatch({
        if (!is.null(data_server)) {
          data_server$getDataReactive()
        } else {
          # Fallback: use reactive data from module
          reactive({
            tibble::tibble()
          })()
        }
      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for selected category
    selected_category <- reactiveVal("course_satisfaction")

    #' Reactive expression for selected question
    selected_question <- reactiveVal(NULL)

    #' Reactive expression for question count label
    question_count_label <- reactive({
      cat <- selected_category()
      n_questions <- length(QUESTION_GROUPS[[cat]])
      paste0(n_questions, " questions")
    })

    # =============================================================================
    # Category Navigation Handlers
    # =============================================================================

    #' Handle category selection changes
    observeEvent(input$category_select, {
      selected_category(input$category_select)
      selected_question(NULL)  # Reset selected question
    })

    # =============================================================================
    # Question Selector Handlers
    # =============================================================================

    #' Handle question button clicks
    observeEvent(names(QUESTION_GROUPS$course_satisfaction), {
      lapply(names(QUESTION_GROUPS$course_satisfaction), function(q_name) {
        btn_id <- ns(paste0("q_", q_name))
        observeEvent(input[[btn_id]], {
          selected_question(q_name)
        })
      })
    })

    # =============================================================================
    # Statistics Calculation
    # =============================================================================

    #' Reactive expression for question statistics
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

        # Get the column name for the selected question
        q_col <- QUESTION_GROUPS[[selected_category()]][[q_key]]

        # Extract Likert values
        likert_values <- data %>%
          dplyr::select(dplyr::all_of(q_col)) %>%
          dplyr::mutate(
            value = extract_likert_value(dplyr::all_of(q_col))
          ) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::pull(value)

        # Calculate descriptive statistics
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

    #' Reactive expression for section comparison data
    section_comparison_data <- reactive({
      tryCatch({
        data <- filtered_data()
        q_key <- selected_question()

        if (nrow(data) == 0 || is.null(q_key)) {
          return(tibble::tibble())
        }

        # Get the column name for the selected question
        q_col <- QUESTION_GROUPS[[selected_category()]][[q_key]]

        # Extract Likert values and section for each row
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

    # =============================================================================
    # Stats Table Output
    # =============================================================================

    #' Render statistics table
    output$stats_table <- DT::renderDataTable({
      stats <- question_stats()

      # Format statistics for display
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

    #' Update question count label
    output$question_count_label <- renderText({
      question_count_label()
    })

    # =============================================================================
    # Histogram Output
    # =============================================================================

    #' Render Likert histogram
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

      # Get the column name for the selected question
      q_col <- QUESTION_GROUPS[[selected_category()]][[q_key]]

      # Count responses by Likert value
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

      # Create Likert labels
      likert_labels <- LIKERT_SCALE

      # Create Likert colors
      likert_colors <- LIKERT_COLORS

      # Create histogram
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

    #' Render section comparison histogram
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

      # Create Likert labels
      likert_labels <- LIKERT_SCALE

      # Create Likert colors
      likert_colors <- LIKERT_COLORS

      # Create histogram with facets
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

    # =============================================================================
    # Section Comparison Toggle Handler
    # =============================================================================

    #' Handle section comparison toggle
    observeEvent(input$compare_sections, {
      # Toggle visibility of section comparison panel
      if (input$compare_sections) {
        shinyjs::show("section-comparison-panel")
      } else {
        shinyjs::hide("section-comparison-panel")
      }
    })

    # Return reactive expressions for use by other modules
    return(list(
      selected_category = selected_category,
      selected_question = selected_question,
      question_stats = question_stats,
      section_comparison_data = section_comparison_data
    ))
  })
}
