# Learning Elements Tab Server Logic
# Server-side logic for the Learning Elements tab including reactive filtering,
# plot rendering, and insights generation

#' Setup Learning Elements Tab Server
#'
#' Configures all reactive values, observers, and render functions for the Learning Elements tab.
#' This function should be called within the main server function.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param df Reactive expression returning the survey data
#' @export
setup_learning_elements_tab <- function(input, output, session, df) {
  ns <- session$ns
  
  # Reactive values for comparison controls
  rv <- reactiveValues(
    section_filter = "all",
    experience_filter = "all",
    preference_filter = "all"
  )
  
  # Observe section filter changes
  observeEvent(input$learning_elements_comparison_section_filter, {
    req(input$learning_elements_comparison_section_filter)
    if ("all" %in% input$learning_elements_comparison_section_filter) {
      rv$section_filter <- "all"
    } else {
      rv$section_filter <- input$learning_elements_comparison_section_filter
    }
  })
  
  # Observe experience filter changes
  observeEvent(input$learning_elements_comparison_experience_filter, {
    req(input$learning_elements_comparison_experience_filter)
    if ("all" %in% input$learning_elements_comparison_experience_filter) {
      rv$experience_filter <- "all"
    } else {
      rv$experience_filter <- input$learning_elements_comparison_experience_filter
    }
  })
  
  # Observe preference filter changes
  observeEvent(input$learning_elements_comparison_preference_filter, {
    req(input$learning_elements_comparison_preference_filter)
    if ("all" %in% input$learning_elements_comparison_preference_filter) {
      rv$preference_filter <- "all"
    } else {
      rv$preference_filter <- input$learning_elements_comparison_preference_filter
    }
  })
  
  # Handle reset filters button
  observeEvent(input$learning_elements_comparison_reset_filters, {
    updateSelectInput(session, "learning_elements_comparison_section_filter", selected = "all")
    updateSelectInput(session, "learning_elements_comparison_experience_filter", selected = "all")
    updateSelectInput(session, "learning_elements_comparison_preference_filter", selected = "all")
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
  
  # Define learning element columns
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
  
  # Render element titles
  for (i in seq_along(learning_columns)) {
    output[[paste0("learning_element_", i, "_title")]] <- renderText({
      get_column_display_name(learning_columns[i])
    })
  }
  
  # Render learning elements rankings
  output$learning_elements_rankings <- renderPlot({
    data <- filtered_data()
    result <- generate_learning_elements_ranking_plot(data, title = "Element Rankings (Average Contribution)")
    result$plot
  })
  
  # Render element distribution
  output$learning_elements_distribution <- renderPlot({
    data <- filtered_data()
    result <- generate_element_distribution_plot(data, title = "Element Contribution Distribution")
    result$plot
  })
  
  # Render individual element plots
  for (i in seq_along(learning_columns)) {
    local({
      idx <- i
      col <- learning_columns[idx]
      output[[paste0("learning_element_", idx)]] <- renderPlot({
        data <- filtered_data()
        result <- generate_single_element_plot(data, col)
        result$plot
      })
    })
  }
  
  # Render correlation heatmap
  output$learning_elements_correlation <- renderPlot({
    data <- filtered_data()
    result <- generate_elements_correlation_heatmap(data, title = "Learning Elements Correlation Matrix")
    result$plot
  })
  
  # Render section comparison
  output$learning_elements_section_comparison <- renderPlot({
    data <- filtered_data()
    result <- generate_elements_comparison_plot(data, "section", title = "Element Scores by Section")
    result$plot
  })
  
  # Render experience comparison
  output$learning_elements_experience_comparison <- renderPlot({
    data <- filtered_data()
    result <- generate_elements_comparison_plot(data, "prior_experience", 
                                                  title = "Element Scores by Experience Level")
    result$plot
  })
  
  # Generate insights
  output$learning_elements_insights <- renderUI({
    data <- filtered_data()
    
    if (is.null(data)) {
      return(div(class = "insights-panel",
                 h4("Insights", class = "insights-title"),
                 p("No data available for insights.", class = "insights-text")))
    }
    
    insights <- list()
    
    # Calculate average scores for each element
    available_cols <- learning_columns[learning_columns %in% names(data)]
    if (length(available_cols) > 0) {
      element_scores <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = everything(), names_to = "element", values_to = "avg_score") %>%
        arrange(desc(avg_score))
      
      # Top contributing element
      top_element <- element_scores$element[1]
      top_score <- round(element_scores$avg_score[1], 2)
      insights <- c(insights, list(
        tags$li(
          class = "insight-item",
          paste0("Highest contributing element: ", get_column_display_name(top_element),
                 " (", top_score, "/5.0)")
        )
      ))
      
      # Lowest contributing element
      if (nrow(element_scores) > 1) {
        bottom_element <- element_scores$element[nrow(element_scores)]
        bottom_score <- round(element_scores$avg_score[nrow(element_scores)], 2)
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0("Lowest contributing element: ", get_column_display_name(bottom_element),
                   " (", bottom_score, "/5.0)")
          )
        ))
      }
      
      # Overall average
      overall_avg <- round(mean(element_scores$avg_score, na.rm = TRUE), 2)
      insights <- c(insights, list(
        tags$li(
          class = "insight-item",
          paste0("Overall contribution score: ", overall_avg, "/5.0")
        )
      ))
      
      # High contribution elements (score >= 4.0)
      high_contrib <- element_scores %>%
        filter(avg_score >= 4.0) %>%
        nrow()
      
      if (high_contrib > 0) {
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0(high_contrib, " elements rated as highly contributing (>= 4.0)")
          )
        ))
      }
    }
    
    div(class = "insights-panel",
        h4("Key Insights", class = "insights-title"),
        ul(class = "insights-list", insights))
  })
}
