homeUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(
          class = "response-overview",
          h3("Response Overview", class = "section-title"),
          div(
            class = "response-counter",
            h4("Total Responses", class = "counter-label"),
            span(class = "counter-value", id = ns("total_responses"))
          ),
          div(
            class = "section-breakdown",
            h4("Responses per Section", class = "chart-title"),
            girafeOutput(ns("section_breakdown_chart"),
              height = "400px"
            ),
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

homeServer <- function(id, data_server = NULL, filter_server = NULL) {
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

    total_responses <- reactive({
      tryCatch({
        data <- filtered_data()
        nrow(data)
      }, error = function(e) {
        0
      })
    })

    section_breakdown_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        section_counts <- data %>%
          dplyr::count(dplyr::all_of(COL_SECTION), sort = TRUE)

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

    learning_preference_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        pref_counts <- data %>%
          dplyr::count(dplyr::all_of(COL_LEARNING_PREF), sort = TRUE)

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

    programming_experience_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        exp_counts <- data %>%
          dplyr::count(dplyr::all_of(COL_EXPERIENCE), sort = TRUE)

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

    course_satisfaction_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        sat_data <- data %>%
          dplyr::select(dplyr::all_of(QUESTION_GROUPS$course_satisfaction)) %>%
          dplyr::summarise(across(everything(), mean, na.rm = TRUE))

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

    discord_engagement_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        discord_responses <- data %>%
          dplyr::select(dplyr::all_of(COL_DISCORD)) %>%
          tidyr::separate_rows(dplyr::all_of(COL_DISCORD), sep = ";", convert = TRUE) %>%
          dplyr::filter(!is.na(.)) %>%
          dplyr::count(.id = TRUE)

        total <- nrow(discord_responses)
        engagement_data <- discord_responses %>%
          dplyr::count(.id, sort = TRUE) %>%
          dplyr::mutate(
            percentage = round(n / total * 100, 1)
          )

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

    learning_methods_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        methods_data <- data %>%
          dplyr::select(dplyr::all_of(QUESTION_GROUPS$learning_methods)) %>%
          dplyr::summarise(across(everything(), mean, na.rm = TRUE))

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

    community_connection_data <- reactive({
      tryCatch({
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        conn_data <- data %>%
          dplyr::select(dplyr::all_of(QUESTION_GROUPS$community_belonging)) %>%
          dplyr::summarise(across(everything(), mean, na.rm = TRUE))

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

    output$total_responses <- renderText({
      total_responses()
    })

    output$selected_section_display <- renderText({
      sec <- selected_section_display()
      if (sec == "" || is.null(sec)) {
        "All Sections"
      } else {
        sec
      }
    })

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

    observeEvent(input$section_chart_click, {
      tryCatch({
        click_data <- input$section_chart_click

        if (!is.null(click_data) && !is.na(click_data$x)) {
          section_data <- section_breakdown_data()
          if (nrow(section_data) > 0) {
            clicked_idx <- which.min(abs(section_data$label - click_data$x))
            selected_section <- section_data$label[clicked_idx]

            if (selected_section != "No Section") {
              if (!is.null(filter_server)) {
                filter_server$updateSelectedSection(selected_section)
              }
            }
          }
        }
      }, error = function(e) {
      })
    })

    observeEvent(input$reset_filter, {
      tryCatch({
        if (!is.null(filter_server)) {
          filter_server$resetFilter()
        }
      }, error = function(e) {
      })
    })

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
