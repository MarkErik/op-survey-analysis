# Community & Belonging Tab Server Logic
# Server-side logic for the Community & Belonging tab including reactive filtering,
# plot rendering, and insights generation

#' Setup Community Tab Server
#'
#' Configures all reactive values, observers, and render functions for the Community & Belonging tab.
#' This function should be called within the main server function.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param df Reactive expression returning the survey data
#' @export
setup_community_tab <- function(input, output, session, df) {
  ns <- session$ns
  
  # Reactive values for comparison controls
  rv <- reactiveValues(
    section_filter = "all",
    experience_filter = "all",
    preference_filter = "all"
  )
  
  # Observe section filter changes
  observeEvent(input$community_comparison_section_filter, {
    req(input$community_comparison_section_filter)
    if ("all" %in% input$community_comparison_section_filter) {
      rv$section_filter <- "all"
    } else {
      rv$section_filter <- input$community_comparison_section_filter
    }
  })
  
  # Observe experience filter changes
  observeEvent(input$community_comparison_experience_filter, {
    req(input$community_comparison_experience_filter)
    if ("all" %in% input$community_comparison_experience_filter) {
      rv$experience_filter <- "all"
    } else {
      rv$experience_filter <- input$community_comparison_experience_filter
    }
  })
  
  # Observe preference filter changes
  observeEvent(input$community_comparison_preference_filter, {
    req(input$community_comparison_preference_filter)
    if ("all" %in% input$community_comparison_preference_filter) {
      rv$preference_filter <- "all"
    } else {
      rv$preference_filter <- input$community_comparison_preference_filter
    }
  })
  
  # Handle reset filters button
  observeEvent(input$community_comparison_reset_filters, {
    updateSelectInput(session, "community_comparison_section_filter", selected = "all")
    updateSelectInput(session, "community_comparison_experience_filter", selected = "all")
    updateSelectInput(session, "community_comparison_preference_filter", selected = "all")
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
  
  # Define belonging statement columns
  belonging_columns <- c(
    "how_much_do_you_agree_with_the_following_statements_1",
    "how_much_do_you_agree_with_the_following_statements_2",
    "how_much_do_you_agree_with_the_following_statements_3",
    "how_much_do_you_agree_with_the_following_statements_4",
    "how_much_do_you_agree_with_the_following_statements_5"
  )
  
  # Render statement titles
  for (i in seq_along(belonging_columns)) {
    output[[paste0("belonging_statement_", i, "_title")]] <- renderText({
      get_column_display_name(belonging_columns[i])
    })
  }
  
  # Render belonging score
  output$community_belonging_score <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("N/A")
    }
    
    available_cols <- belonging_columns[belonging_columns %in% names(data)]
    if (length(available_cols) > 0) {
      avg_score <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        summarise(avg = mean(c_across(everything()), na.rm = TRUE)) %>%
        pull(avg)
      return(round(avg_score, 2))
    }
    return("N/A")
  })
  
  # Render strong agreement rate
  output$community_strong_agreement_rate <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("0%")
    }
    
    available_cols <- belonging_columns[belonging_columns %in% names(data)]
    if (length(available_cols) > 0) {
      total_responses <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        pivot_longer(cols = everything(), names_to = "statement", values_to = "response") %>%
        filter(!is.na(response)) %>%
        nrow()
      
      strong_agree <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        pivot_longer(cols = everything(), names_to = "statement", values_to = "response") %>%
        filter(response == 5) %>%
        nrow()
      
      if (total_responses > 0) {
        return(paste0(round(strong_agree / total_responses * 100, 1), "%"))
      }
    }
    return("0%")
  })
  
  # Render Discord engagement
  output$community_discord_engagement <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("0%")
    }
    
    discord_cols <- grep("^discord_", names(data), value = TRUE)
    discord_cols <- setdiff(discord_cols, "discord_custom_response")
    
    if (length(discord_cols) > 0) {
      # Count students who use at least one Discord feature
      discord_users <- data %>%
        select(all_of(discord_cols)) %>%
        rowwise() %>%
        mutate(uses_discord = any(c_across(everything()) == 1, na.rm = TRUE)) %>%
        filter(uses_discord) %>%
        nrow()
      
      total <- nrow(data)
      if (total > 0) {
        return(paste0(round(discord_users / total * 100, 1), "%"))
      }
    }
    return("0%")
  })
  
  # Render belonging statements distribution
  output$belonging_statements_distribution <- renderPlot({
    data <- filtered_data()
    result <- generate_belonging_statements_distribution_plot(data, title = "Belonging Statements Distribution")
    result$plot
  })
  
  # Render individual statement plots
  for (i in seq_along(belonging_columns)) {
    local({
      idx <- i
      col <- belonging_columns[idx]
      output[[paste0("belonging_statement_", idx)]] <- renderPlot({
        data <- filtered_data()
        result <- generate_belonging_statement_plot(data, col)
        result$plot
      })
    })
  }
  
  # Render Discord feature usage
  output$discord_feature_usage <- renderPlot({
    data <- filtered_data()
    result <- generate_discord_usage_plot(data, title = "Discord Feature Usage")
    result$plot
  })
  
  # Render Discord usage patterns
  output$discord_usage_patterns <- renderPlot({
    data <- filtered_data()
    result <- generate_discord_pattern_plot(data, "section", title = "Discord Usage by Section")
    result$plot
  })
  
  # Render section comparison
  output$community_section_comparison <- renderPlot({
    data <- filtered_data()
    result <- generate_belonging_comparison_plot(data, belonging_columns, "section",
                                                  title = "Belonging Scores by Section")
    result$plot
  })
  
  # Render experience comparison
  output$community_experience_comparison <- renderPlot({
    data <- filtered_data()
    result <- generate_belonging_comparison_plot(data, belonging_columns, "prior_experience",
                                                  title = "Belonging Scores by Experience Level")
    result$plot
  })
  
  # Generate insights
  output$community_insights <- renderUI({
    data <- filtered_data()
    
    if (is.null(data)) {
      return(div(class = "insights-panel",
                 h4("Insights", class = "insights-title"),
                 p("No data available for insights.", class = "insights-text")))
    }
    
    insights <- list()
    
    # Calculate average scores for each statement
    available_cols <- belonging_columns[belonging_columns %in% names(data)]
    if (length(available_cols) > 0) {
      statement_scores <- data %>%
        select(all_of(available_cols)) %>%
        mutate(across(everything(), as.numeric)) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = everything(), names_to = "statement", values_to = "avg_score") %>%
        arrange(desc(avg_score))
      
      # Top belonging statement
      top_statement <- statement_scores$statement[1]
      top_score <- round(statement_scores$avg_score[1], 2)
      insights <- c(insights, list(
        tags$li(
          class = "insight-item",
          paste0("Strongest agreement: ", get_column_display_name(top_statement),
                 " (", top_score, "/5.0)")
        )
      ))
      
      # Lowest belonging statement
      if (nrow(statement_scores) > 1) {
        bottom_statement <- statement_scores$statement[nrow(statement_scores)]
        bottom_score <- round(statement_scores$avg_score[nrow(statement_scores)], 2)
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0("Weakest agreement: ", get_column_display_name(bottom_statement),
                   " (", bottom_score, "/5.0)")
          )
        ))
      }
      
      # Overall belonging score
      overall_avg <- round(mean(statement_scores$avg_score, na.rm = TRUE), 2)
      insights <- c(insights, list(
        tags$li(
          class = "insight-item",
          paste0("Overall belonging score: ", overall_avg, "/5.0")
        )
      ))
    }
    
    # Discord usage insight
    discord_cols <- grep("^discord_", names(data), value = TRUE)
    discord_cols <- setdiff(discord_cols, "discord_custom_response")
    
    if (length(discord_cols) > 0) {
      discord_users <- data %>%
        select(all_of(discord_cols)) %>%
        rowwise() %>        mutate(uses_discord = any(c_across(everything()) == 1, na.rm = TRUE)) %>%
        filter(uses_discord) %>%
        nrow()
      
      total <- nrow(data)
      if (total > 0) {
        discord_pct <- round(discord_users / total * 100, 1)
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0(discord_pct, "% of students use Discord")
          )
        ))
      }
      
      # Most popular Discord feature
      feature_usage <- data %>%
        select(all_of(discord_cols)) %>%
        summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = everything(), names_to = "feature", values_to = "count") %>%
        arrange(desc(count))
      
      if (nrow(feature_usage) > 0 && feature_usage$count[1] > 0) {
        top_feature <- feature_usage$feature[1]
        top_feature <- gsub("^discord_", "", top_feature)
        top_feature <- gsub("_", " ", top_feature)
        top_feature <- tools::toTitleCase(top_feature)
        insights <- c(insights, list(
          tags$li(
            class = "insight-item",
            paste0("Most popular Discord feature: ", top_feature)
          )
        ))
      }
    }
    
    div(class = "insights-panel",
        h4("Key Insights", class = "insights-title"),
        ul(class = "insights-list", insights))
  })
}
