# Course Content Tab Server Logic
# Server-side logic for the Course Content tab including reactive filtering,
# plot rendering, and insights generation

#' Setup Course Content Tab Server
#'
#' Configures all reactive values, observers, and render functions for the Course Content tab.
#' This function should be called within the main server function.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param df Reactive expression returning the survey data
#' @export
setup_course_content_tab <- function(input, output, session, df) {
  ns <- session$ns
  
  # Reactive values for comparison controls
  rv <- reactiveValues(
    section_filter = "all",
    experience_filter = "all",
    preference_filter = "all"
  )
  
  # Observe section filter changes
  observeEvent(input$course_content_comparison_section_filter, {
    req(input$course_content_comparison_section_filter)
    if ("all" %in% input$course_content_comparison_section_filter) {
      rv$section_filter <- "all"
    } else {
      rv$section_filter <- input$course_content_comparison_section_filter
    }
  })
  
  # Observe experience filter changes
  observeEvent(input$course_content_comparison_experience_filter, {
    req(input$course_content_comparison_experience_filter)
    if ("all" %in% input$course_content_comparison_experience_filter) {
      rv$experience_filter <- "all"
    } else {
      rv$experience_filter <- input$course_content_comparison_experience_filter
    }
  })
  
  # Observe preference filter changes
  observeEvent(input$course_content_comparison_preference_filter, {
    req(input$course_content_comparison_preference_filter)
    if ("all" %in% input$course_content_comparison_preference_filter) {
      rv$preference_filter <- "all"
    } else {
      rv$preference_filter <- input$course_content_comparison_preference_filter
    }
  })
  
  # Handle reset filters button
  observeEvent(input$course_content_comparison_reset_filters, {
    updateSelectInput(session, "course_content_comparison_section_filter", selected = "all")
    updateSelectInput(session, "course_content_comparison_experience_filter", selected = "all")
    updateSelectInput(session, "course_content_comparison_preference_filter", selected = "all")
    rv$section_filter <- "all"
    rv$experience_filter <- "all"
    rv$preference_filter <- "all"
  })
  
  # Reactive filtered data based on comparison controls
  filtered_data <- reactive({
    data <- df()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Apply section filter
    if (!is.null(rv$section_filter) && rv$section_filter != "all") {
      if (length(rv$section_filter) == 1) {
        data <- data[data$section == rv$section_filter, ]
      } else {
        data <- data[data$section %in% rv$section_filter, ]
      }
    }
    
    # Apply experience filter
    if (!is.null(rv$experience_filter) && rv$experience_filter != "all") {
      if (length(rv$experience_filter) == 1) {
        exp_mapping <- list(
          "none" = "No experience at all",
          "some" = "Took programming course before (either in school, or online tutorials)",
          "high" = "Highly experienced (comfortable writing own programs)"
        )
        data <- data[data$prior_experience == exp_mapping[[rv$experience_filter]], ]
      } else {
        exp_mapping <- list(
          "none" = "No experience at all",
          "some" = "Took programming course before (either in school, or online tutorials)",
          "high" = "Highly experienced (comfortable writing own programs)"
        )
        exp_values <- unlist(exp_mapping[rv$experience_filter])
        data <- data[data$prior_experience %in% exp_values, ]
      }
    }
    
    # Apply preference filter
    if (!is.null(rv$preference_filter) && rv$preference_filter != "all") {
      if (length(rv$preference_filter) == 1) {
        data <- data[data$learning_preference == rv$preference_filter, ]
      } else {
        data <- data[data$learning_preference %in% rv$preference_filter, ]
      }
    }
    
    return(data)
  })
  
  # Define agreement statement columns
  agreement_columns <- c(
    "how_much_do_you_agree_with_the_statement_1",
    "how_much_do_you_agree_with_the_statement_2",
    "how_much_do_you_agree_with_the_statement_3",
    "how_much_do_you_agree_with_the_statement_4",
    "how_much_do_you_agree_with_the_statement_5",
    "how_much_do_you_agree_with_the_statement_6"
  )
  
  # Render statement titles
  for (i in seq_along(agreement_columns)) {
    output[[paste0("course_statement_", i, "_title")]] <- renderText({
      get_column_display_name(agreement_columns[i])
    })
  }
  
  # Render agreement heatmap
  output$course_agreement_heatmap <- renderPlot({
    data <- filtered_data()
    result <- generate_agreement_heatmap(data, title = "Agreement Statement Distribution")
    result$plot
  })
  
  # Render agreement rankings
  output$course_agreement_rankings <- renderPlot({
    data <- filtered_data()
    result <- generate_agreement_rankings_plot(data, title = "Statement Rankings (Average Score)")
    result$plot
  })
  
  # Render individual statement plots
  for (i in seq_along(agreement_columns)) {
    local({
      idx <- i
      col <- agreement_columns[idx]
      output[[paste0("course_statement_", idx)]] <- renderPlot({
        data <- filtered_data()
        result <- generate_agreement_statement_plot(data, col)
        result$plot
      })
    })
  }
  
  # Render learning preference distribution
  output$learning_preference_distribution <- renderPlot({
    data <- filtered_data()
    result <- generate_learning_preference_plot(data, group_by = NULL, title = "Learning Format Preference")
    result$plot
  })
  
  # Render expectations met gauge (placeholder - would need actual expectations data)
  output$expectations_met_gauge <- renderPlot({
    data <- filtered_data()
    # Create a simple gauge based on average agreement score
    if (is.null(data)) {
      p <- ggplot() +
        geom_blank() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
      return(p)
    }
    
    # Calculate average from agreement statements
    available_cols <- agreement_columns[agreement_columns %in% names(data)]
    if (length(available_cols) > 0) {
      avg_score <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        summarise(avg = mean(c_across(everything()), na.rm = TRUE)) %>%
        pull(avg)
      
      score_percent <- (avg_score - 1) / 4 * 100
      gauge_data <- data.frame(
        category = c("Score", "Remaining"),
        value = c(score_percent, 100 - score_percent)
      )
      
      p <- ggplot(gauge_data, aes(x = category, y = value, fill = category)) +
        geom_col(width = 0.5, color = COLORS$border, linewidth = 0.5) +
        geom_text(aes(label = ifelse(category == "Score", 
                                     paste0(round(avg_score, 2), " / 5.0"), "")),
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
        labs(y = "Percentage", title = "Expectations Met")
    } else {
      p <- ggplot() +
        geom_blank() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No agreement data available", size = 5)
    }
    p
  })
  
  # Render section comparison
  output$course_section_comparison <- renderPlot({
    data <- filtered_data()
    result <- generate_agreement_comparison_plot(data, agreement_columns, "section", 
                                                  title = "Agreement Scores by Section")
    result$plot
  })
  
  # Render experience comparison
  output$course_experience_comparison <- renderPlot({
    data <- filtered_data()
    result <- generate_agreement_comparison_plot(data, agreement_columns, "prior_experience",
                                                  title = "Agreement Scores by Experience Level")
    result$plot
  })
  
  # Generate insights
  output$course_content_insights <- renderUI({
    data <- filtered_data()
    
    if (is.null(data)) {
      return(div(class = "insights-panel",
                 h4("Insights", class = "insights-title"),
                 p("No data available for insights.", class = "insights-text")))
    }
    
    insights <- list()
    
    # Calculate average scores for each statement
    available_cols <- agreement_columns[agreement_columns %in% names(data)]
    if (length(available_cols) > 0) {
      statement_scores <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = everything(), names_to = "statement", values_to = "avg_score") %>%
        arrange(desc(avg_score))
      
      # Top performing statement
      top_statement <- statement_scores$statement[1]
      top_score <- round(statement_scores$avg_score[1], 2)
      insights <- c(insights, list(
        tags$li(
          class = "insight-item",
          paste0("Highest agreement: ", get_column_display_name(top_statement), 
                 " (", top_score, "/5.0)")
        )
      ))
      
      # Lowest performing statement
      if (nrow(statement_scores) > 1) {
        bottom_statement <- statement_scores$statement[nrow(statement_scores)]
        bottom_score <- round(statement_scores$avg_score[nrow(statement_scores)], 2)
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0("Lowest agreement: ", get_column_display_name(bottom_statement),
                   " (", bottom_score, "/5.0)")
          )
        ))
      }
      
      # Overall average
      overall_avg <- round(mean(statement_scores$avg_score, na.rm = TRUE), 2)
      insights <- c(insights, list(
        tags$li(
          class = "insight-item",
          paste0("Overall agreement score: ", overall_avg, "/5.0")
        )
      ))
    }
    
    # Learning preference insight
    if ("learning_preference" %in% names(data)) {
      pref_data <- data %>%
        filter(!is.na(learning_preference)) %>%
        count(learning_preference) %>%
        arrange(desc(n))
      
      if (nrow(pref_data) > 0) {
        top_pref <- pref_data$learning_preference[1]
        pref_pct <- round(pref_data$n[1] / sum(pref_data$n) * 100, 1)
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0("Most preferred learning format: ", top_pref, " (", pref_pct, "%)")
          )
        ))
      }
    }
    
    div(class = "insights-panel",
        h4("Key Insights", class = "insights-title"),
        ul(class = "insights-list", insights))
  })
}
