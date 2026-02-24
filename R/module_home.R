# R/module_home.R
# Home tab module for the CPSC Experience Survey Explorer
# Provides UI and server functions for the Home tab with response overview and visualizations

# =============================================================================
# UI Module - Home Tab
# =============================================================================

#' Home UI module function
#'
#' Creates the Home tab UI with response overview and six visualization panels.
#'
#' @param id Character module ID for namespacing
#' @return UI element for the Home tab
#' @export
homeUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Response Overview Section
    fluidRow(
      column(12,
        div(
          class = "response-overview",
          h3("Response Overview", class = "section-title"),
          # Total Responses Counter
          div(
            class = "response-counter",
            h4("Total Responses", class = "counter-label"),
            span(class = "counter-value", id = ns("total_responses"))
          ),
          # Section Breakdown Chart
          div(
            class = "section-breakdown",
            h4("Responses per Section", class = "chart-title"),
            girafeOutput(ns("section_breakdown_chart"),
              height = "400px"
            ),
            # Section Filter Display
            div(
              class = "section-filter-display",
              h5("Selected Section:", class = "filter-label"),
              span(class = "filter-value", id = ns("selected_section_display")),
              actionButton(ns("reset_filter"), "Reset Filter",
                class = "reset-filter-btn",
                icon = icon("undo")
              )
            )
          )
        )
      )
    ),

    # Six Visualization Panels
    # 1. Learning Preference Distribution
    fluidRow(
      column(12,
        div(
          class = "viz-panel",
          h4("Learning Preference Distribution", class = "viz-title"),
          plotOutput(ns("learning_preference_chart"),
            click = ns("learning_pref_click"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    ),

    # 2. Prior Programming Experience
    fluidRow(
      column(12,
        div(
          class = "viz-panel",
          h4("Prior Programming Experience", class = "viz-title"),
          plotOutput(ns("programming_experience_chart"),
            click = ns("programming_exp_click"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    ),

    # 3. Course Satisfaction Overview
    fluidRow(
      column(12,
        div(
          class = "viz-panel",
          h4("Course Satisfaction Overview", class = "viz-title"),
          plotOutput(ns("course_satisfaction_chart"),
            click = ns("course_sat_click"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    ),

    # 4. Discord Engagement Metrics
    fluidRow(
      column(12,
        div(
          class = "viz-panel",
          h4("Discord Engagement Metrics", class = "viz-title"),
          plotOutput(ns("discord_engagement_chart"),
            click = ns("discord_eng_click"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    ),

    # 5. Most Valuable Learning Methods
    fluidRow(
      column(12,
        div(
          class = "viz-panel",
          h4("Most Valuable Learning Methods", class = "viz-title"),
          plotOutput(ns("learning_methods_chart"),
            click = ns("learning_methods_click"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    ),

    # 6. Community Connection Scores
    fluidRow(
      column(12,
        div(
          class = "viz-panel",
          h4("Community Connection Scores", class = "viz-title"),
          plotOutput(ns("community_connection_chart"),
            click = ns("community_conn_click"),
            height = "350px",
            tooltip = TRUE
          )
        )
      )
    )
  )
}

# =============================================================================
# Server Module - Home Tab
# =============================================================================

#' Home module server function
#'
#' Provides reactive data expressions and click handlers for the Home tab.
#'
#' @param id Character module ID for namespacing
#' @param data_server Reactive data server module (optional)
#' @param filter_server Reactive filter server module (optional)
#' @return List of reactive expressions and outputs for the Home tab
#' @export
homeServer <- function(id, data_server = NULL, filter_server = NULL) {
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

    #' Reactive expression for total responses
    total_responses <- reactive({
      tryCatch({
        data <- filtered_data()
        nrow(data)
      }, error = function(e) {
        0
      })
    })

    #' Reactive expression for section breakdown data
    section_breakdown_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Count responses per section
        section_counts <- data %>%
          dplyr::count(dplyr::all_of(COL_SECTION), sort = TRUE)

        # Add percentage column
        total <- sum(section_counts$n)
        section_counts <- section_counts %>%
          dplyr::mutate(
            percentage = round(n / total * 100, 1),
            label = ifelse(is.na(COL_SECTION) | COL_SECTION == "", "No Section", COL_SECTION)
          )

        return(section_counts)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for learning preference data
    learning_preference_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Count learning preferences
        pref_counts <- data %>%
          dplyr::count(dplyr::all_of(COL_LEARNING_PREF), sort = TRUE)

        # Add percentage column
        total <- sum(pref_counts$n)
        pref_counts <- pref_counts %>%
          dplyr::mutate(
            percentage = round(n / total * 100, 1)
          )

        return(pref_counts)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for programming experience data
    programming_experience_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Count programming experience levels
        exp_counts <- data %>%
          dplyr::count(dplyr::all_of(COL_EXPERIENCE), sort = TRUE)

        # Add percentage column
        total <- sum(exp_counts$n)
        exp_counts <- exp_counts %>%
          dplyr::mutate(
            percentage = round(n / total * 100, 1)
          )

        return(exp_counts)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for course satisfaction data
    course_satisfaction_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Calculate average for each satisfaction statement
        sat_data <- data %>%
          dplyr::select(dplyr::all_of(QUESTION_GROUPS$course_satisfaction)) %>%
          dplyr::summarise(across(everything(), mean, na.rm = TRUE))

        # Convert to long format
        sat_long <- sat_data %>%
          dplyr::pivot_longer(
            cols = dplyr::all_of(QUESTION_GROUPS$course_satisfaction),
            names_to = "statement",
            values_to = "average"
          ) %>%
          dplyr::arrange(dplyr::desc(average))

        return(sat_long)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for Discord engagement data
    discord_engagement_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Parse Discord multi-select responses
        discord_responses <- data %>%
          dplyr::select(dplyr::all_of(COL_DISCORD)) %>%
          tidyr::separate_rows(dplyr::all_of(COL_DISCORD), sep = ";", convert = TRUE) %>%
          dplyr::filter(!is.na(.)) %>%
          dplyr::count(.id = TRUE)

        # Calculate percentages for each option
        total <- nrow(discord_responses)
        engagement_data <- discord_responses %>%
          dplyr::count(.id, sort = TRUE) %>%
          dplyr::mutate(
            percentage = round(n / total * 100, 1)
          )

        # Map to specific metrics
        metrics <- list(
          "Joined Discord" = "I have joined the class Discord",
          "Active on Discord" = "I am active in the class Discord",
          "Useful for learning" = "It is really useful for me for learning"
        )

        result <- tibble::tibble(
          metric = character(),
          percentage = numeric()
        )

        for (metric_name in names(metrics)) {
          option_label <- metrics[[metric_name]]
          count <- engagement_data %>%
            dplyr::filter(.id == option_label) %>%
            dplyr::pull(n) %>%
            sum()
          result <- result %>%
            dplyr::bind_rows(tibble::tibble(
              metric = metric_name,
              percentage = round(count / total * 100, 1)
            ))
        }

        return(result)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for learning methods data
    learning_methods_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Calculate average for each learning method
        methods_data <- data %>%
          dplyr::select(dplyr::all_of(QUESTION_GROUPS$learning_methods)) %>%
          dplyr::summarise(across(everything(), mean, na.rm = TRUE))

        # Convert to long format and get top 7
        methods_long <- methods_data %>%
          dplyr::pivot_longer(
            cols = dplyr::all_of(QUESTION_GROUPS$learning_methods),
            names_to = "method",
            values_to = "average"
          ) %>%
          dplyr::arrange(dplyr::desc(average)) %>%
          dplyr::slice_head(n = 7)

        return(methods_long)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for community connection data
    community_connection_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Calculate average for each community statement
        conn_data <- data %>%
          dplyr::select(dplyr::all_of(QUESTION_GROUPS$community_belonging)) %>%
          dplyr::summarise(across(everything(), mean, na.rm = TRUE))

        # Convert to long format
        conn_long <- conn_data %>%
          dplyr::pivot_longer(
            cols = dplyr::all_of(QUESTION_GROUPS$community_belonging),
            names_to = "statement",
            values_to = "average"
          ) %>%
          dplyr::arrange(dplyr::desc(average))

        return(conn_long)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for selected section display
    selected_section_display <- reactive({
      tryCatch({
        if (!is.null(filter_server)) {
          filter_server$selected_section()
        } else {
          ""
        }
      }, error = function(e) {
        ""
      })
    })

    # =============================================================================
    # Chart Outputs
    # =============================================================================

    #' Render total responses counter
    output$total_responses <- renderText({
      total_responses()
    })

    #' Render selected section display
    output$selected_section_display <- renderText({
      sec <- selected_section_display()
      if (sec == "" || is.null(sec)) {
        "All Sections"
      } else {
        sec
      }
    })

    #' Render section breakdown chart
    output$section_breakdown_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(section_breakdown_data(), ggplot2::aes(x = label, y = n, fill = label)) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Section: ", label, "<br>Responses: ", n, "<br>Percentage: ", percentage, "%"))) +
          ggplot2::scale_fill_manual(values = COLOR_PALETTE$primary) +
          ggplot2::labs(
            title = "Responses per Section",
            x = "Section",
            y = "Number of Responses"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    #' Render learning preference chart
    output$learning_preference_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(learning_preference_data(), ggplot2::aes(x = dplyr::reorder(dplyr::all_of(COL_LEARNING_PREF), n), y = n, fill = dplyr::reorder(dplyr::all_of(COL_LEARNING_PREF), n))) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Preference: ", dplyr::all_of(COL_LEARNING_PREF), "<br>Responses: ", n, "<br>Percentage: ", percentage, "%"))) +
          ggplot2::scale_fill_manual(values = c("In-person" = COLOR_PALETTE$primary, "Online" = COLOR_PALETTE$secondary, "No preference" = COLOR_PALETTE$neutral)) +
          ggplot2::labs(
            title = "Learning Preference Distribution",
            x = "Learning Preference",
            y = "Number of Responses"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    #' Render programming experience chart
    output$programming_experience_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(programming_experience_data(), ggplot2::aes(x = dplyr::reorder(dplyr::all_of(COL_EXPERIENCE), n), y = n, fill = dplyr::reorder(dplyr::all_of(COL_EXPERIENCE), n))) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Experience: ", dplyr::all_of(COL_EXPERIENCE), "<br>Responses: ", n, "<br>Percentage: ", percentage, "%"))) +
          ggplot2::scale_fill_manual(values = c("Highly experienced" = COLOR_PALETTE$success, "Took programming course before" = COLOR_PALETTE$info, "No experience at all" = COLOR_PALETTE$warning)) +
          ggplot2::labs(
            title = "Prior Programming Experience",
            x = "Programming Experience",
            y = "Number of Responses"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    #' Render course satisfaction chart
    output$course_satisfaction_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(course_satisfaction_data(), ggplot2::aes(x = dplyr::reorder(statement, average), y = average, fill = dplyr::reorder(statement, average))) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Statement: ", statement, "<br>Average Score: ", round(average, 2)))) +
          ggplot2::scale_fill_manual(values = LIKERT_COLORS) +
          ggplot2::labs(
            title = "Course Satisfaction Overview",
            x = "Statement",
            y = "Average Score (1-5)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    #' Render Discord engagement chart
    output$discord_engagement_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(discord_engagement_data(), ggplot2::aes(x = dplyr::reorder(metric, percentage), y = percentage, fill = dplyr::reorder(metric, percentage))) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Metric: ", metric, "<br>Percentage: ", percentage, "%"))) +
          ggplot2::scale_fill_manual(values = c("Joined Discord" = COLOR_PALETTE$primary, "Active on Discord" = COLOR_PALETTE$secondary, "Useful for learning" = COLOR_PALETTE$success)) +
          ggplot2::labs(
            title = "Discord Engagement Metrics",
            x = "Metric",
            y = "Percentage (%)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    #' Render learning methods chart
    output$learning_methods_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(learning_methods_data(), ggplot2::aes(x = dplyr::reorder(method, average), y = average, fill = dplyr::reorder(method, average))) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Method: ", method, "<br>Average Score: ", round(average, 2)))) +
          ggplot2::scale_fill_manual(values = LIKERT_COLORS) +
          ggplot2::labs(
            title = "Most Valuable Learning Methods",
            x = "Learning Method",
            y = "Average Score (1-5)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    #' Render community connection chart
    output$community_connection_chart <- renderGirafe({
      girafe(
        ggplot2::ggplot(community_connection_data(), ggplot2::aes(x = dplyr::reorder(statement, average), y = average, fill = dplyr::reorder(statement, average))) +
          ggplot2::geom_bar_interactive(ggplot2::aes(tooltip = paste0("Statement: ", statement, "<br>Average Score: ", round(average, 2)))) +
          ggplot2::scale_fill_manual(values = LIKERT_COLORS) +
          ggplot2::labs(
            title = "Community Connection Scores",
            x = "Statement",
            y = "Average Score (1-5)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.minor = ggplot2::element_blank()
          ),
        width = "100%",
        height = "100%",
        zoom_min = 0.5,
        zoom_max = 3,
        zoom_ondblclick = FALSE
      )
    })

    # =============================================================================
    # Click Handlers for Section Filtering
    # =============================================================================

    #' Handle section chart clicks for filtering
    observeEvent(input$section_chart_click, {
      tryCatch({
        click_data <- input$section_chart_click

        if (!is.null(click_data) && !is.na(click_data$x)) {
          # Find the section label from the click data
          section_data <- section_breakdown_data()
          if (nrow(section_data) > 0) {
            # Get the section name from the clicked position
            clicked_idx <- which.min(abs(section_data$label - click_data$x))
            selected_section <- section_data$label[clicked_idx]

            # Update filter if a section was clicked (not "No Section")
            if (selected_section != "No Section") {
              if (!is.null(filter_server)) {
                filter_server$updateSelectedSection(selected_section)
              }
            }
          }
        }
      }, error = function(e) {
        # Silently handle errors
      })
    })

    #' Handle reset filter button click
    observeEvent(input$reset_filter, {
      tryCatch({
        if (!is.null(filter_server)) {
          filter_server$resetFilter()
        }
      }, error = function(e) {
        # Silently handle errors
      })
    })

    # Return reactive expressions for use by other modules
    return(list(
      filtered_data = filtered_data,
      total_responses = total_responses,
      section_breakdown_data = section_breakdown_data,
      learning_preference_data = learning_preference_data,
      programming_experience_data = programming_experience_data,
      course_satisfaction_data = course_satisfaction_data,
      discord_engagement_data = discord_engagement_data,
      learning_methods_data = learning_methods_data,
      community_connection_data = community_connection_data,
      selected_section_display = selected_section_display
    ))
  })
}
