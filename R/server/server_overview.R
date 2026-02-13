# Overview Tab Server Logic
# Server-side logic for the Overview tab including reactive filtering,
# plot rendering, and insights generation

#' Setup Overview Tab Server
#'
#' Configures all reactive values, observers, and render functions for the Overview tab.
#' This function should be called within the main server function.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param df Reactive expression returning the survey data
#' @export
setup_overview_tab <- function(input, output, session, df) {
  ns <- session$ns
  
  # Reactive values for comparison controls
  rv <- reactiveValues(
    section_filter = "all",
    experience_filter = "all",
    preference_filter = "all"
  )
  
  # Observe section filter changes
  observeEvent(input$overview_comparison_section_filter, {
    req(input$overview_comparison_section_filter)
    if ("all" %in% input$overview_comparison_section_filter) {
      rv$section_filter <- "all"
    } else {
      rv$section_filter <- input$overview_comparison_section_filter
    }
  })
  
  # Observe experience filter changes
  observeEvent(input$overview_comparison_experience_filter, {
    req(input$overview_comparison_experience_filter)
    if ("all" %in% input$overview_comparison_experience_filter) {
      rv$experience_filter <- "all"
    } else {
      rv$experience_filter <- input$overview_comparison_experience_filter
    }
  })
  
  # Observe preference filter changes
  observeEvent(input$overview_comparison_preference_filter, {
    req(input$overview_comparison_preference_filter)
    if ("all" %in% input$overview_comparison_preference_filter) {
      rv$preference_filter <- "all"
    } else {
      rv$preference_filter <- input$overview_comparison_preference_filter
    }
  })
  
  # Handle reset filters button
  observeEvent(input$overview_comparison_reset_filters, {
    updateSelectInput(session, "overview_comparison_section_filter", selected = "all")
    updateSelectInput(session, "overview_comparison_experience_filter", selected = "all")
    updateSelectInput(session, "overview_comparison_preference_filter", selected = "all")
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
  
  # Render key metrics
  output$overview_total_responses <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("0")
    }
    format(nrow(data), big.mark = ",")
  })
  
  output$overview_unique_sections <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("0")
    }
    unique_sections <- unique(data$section)
    unique_sections <- unique_sections[!is.na(unique_sections)]
    length(unique_sections)
  })
  
  output$overview_response_rate <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("0%"
    }
    # Estimate response rate (this is a placeholder - actual calculation would need enrollment data)
    paste0("100%", " (", nrow(data), " responses)")
  })
  
  output$overview_avg_satisfaction <- renderText({
    data <- filtered_data()
    if (is.null(data)) {
      return("N/A"
    }
    
    # Calculate average satisfaction from course agreement statements
    agreement_columns <- c(
      "how_much_do_you_agree_with_the_statement_1",
      "how_much_do_you_agree_with_the_statement_2",
      "how_much_do_you_agree_with_the_statement_3",
      "how_much_do_you_agree_with_the_statement_4",
      "how_much_do_you_agree_with_the_statement_5",
      "how_much_do_you_agree_with_the_statement_6"
    )
    
    available_columns <- agreement_columns[agreement_columns %in% names(data)]
    
    if (length(available_columns) == 0) {
      return("N/A"
    }
    
    avg_score <- data %>%
      select(all_of(available_columns)) %>%
      summarise(across(everything(), mean, na.rm = TRUE)) %>%
      summarise(mean = mean(across(everything()), na.rm = TRUE)) %>%
      pull(mean)
    
    if (is.na(avg_score)) {
      return("N/A"
    }
    
    # Convert to a satisfaction score out of 5
    paste0(round(avg_score, 1), "/5")
  })
  
  # Render section distribution plot
  output$overview_section_distribution <- renderPlot({
    data <- filtered_data()
    result <- generate_section_distribution_plot(data)
    result$plot
  })
  
  # Render experience distribution plot
  output$overview_experience_distribution <- renderPlot({
    data <- filtered_data()
    result <- generate_experience_distribution_plot(data)
    result$plot
  })
  
  # Render learning preference plot
  output$overview_learning_preference <- renderPlot({
    data <- filtered_data()
    result <- generate_learning_preference_plot(data)
    result$plot
  })
  
  # Render course agreement overview plot
  output$overview_course_agreement <- renderPlot({
    data <- filtered_data()
    result <- generate_course_agreement_overview_plot(data)
    result$plot
  })
  
  # Generate insights for overview tab
  overview_insights <- reactive({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(list(
        key_findings = "No data available for insights",
        statistical_highlights = "",
        notable_patterns = "",
        recommendations = ""
      ))
    }
    
    insights <- list()
    
    # Key Findings
    total_responses <- nrow(data)
    unique_sections <- length(unique(data$section[!is.na(data$section)]))
    
    # Most common learning preference
    pref_counts <- data %>%
      filter(!is.na(learning_preference)) %>%
      count(learning_preference) %>%
      arrange(desc(n))
    
    top_preference <- if (nrow(pref_counts) > 0) {
      pref_counts$learning_preference[1]
    } else {
      "N/A"
    }
    
    # Most common experience level
    exp_counts <- data %>%
      filter(!is.na(prior_experience)) %>%
      count(prior_experience) %>%
      arrange(desc(n))
    
    top_experience <- if (nrow(exp_counts) > 0) {
      exp_counts$prior_experience[1]
    } else {
      "N/A"
    }
    
    insights$key_findings <- paste0(
      "• Survey collected ", total_responses, " responses across ", unique_sections, " sections.<br>",
      "• Most students prefer ", top_preference, " learning.<br>",
      "• Most students have ", top_experience, "."
    )
    
    # Statistical Highlights
    agreement_columns <- c(
      "how_much_do_you_agree_with_the_statement_1",
      "how_much_do_you_agree_with_the_statement_2",
      "how_much_do_you_agree_with_the_statement_3",
      "how_much_do_you_agree_with_the_statement_4",
      "how_much_do_you_agree_with_the_statement_5",
      "how_much_do_you_agree_with_the_statement_6"
    )
    
    available_columns <- agreement_columns[agreement_columns %in% names(data)]
    
    if (length(available_columns) > 0) {
      avg_agreement <- data %>%
        select(all_of(available_columns)) %>%
        summarise(across(everything(), mean, na.rm = TRUE)) %>%
        summarise(mean = mean(across(everything()), na.rm = TRUE)) %>%
        pull(mean)
      
      if (!is.na(avg_agreement)) {
        agreement_level <- if (avg_agreement >= 4) {
          "Strongly Positive"
        } else if (avg_agreement >= 3) {
          "Moderately Positive"
        } else if (avg_agreement >= 2) {
          "Neutral to Mixed"
        } else {
          "Negative"
        }
        
        insights$statistical_highlights <- paste0(
          "• Average agreement score: ", round(avg_agreement, 2), "/5 (", agreement_level, ").<br>",
          "• Response distribution shows ", 
          round(sum(data$learning_preference == "In-person", na.rm = TRUE) / nrow(data) * 100, 1),
          "% prefer in-person learning."
        )
      } else {
        insights$statistical_highlights <- "• Insufficient data for statistical analysis."
      }
    } else {
      insights$statistical_highlights <- "• No agreement data available."
    }
    
    # Notable Patterns
    patterns <- c()
    
    # Check for section imbalance
    section_counts <- data %>%
      filter(!is.na(section)) %>%
      count(section) %>%
      arrange(desc(n))
    
    if (nrow(section_counts) > 1) {
      max_count <- section_counts$n[1]
      min_count <- section_counts$n[nrow(section_counts)]
      ratio <- max_count / min_count
      
      if (ratio > 2) {
        patterns <- c(patterns, paste0(
          "• Section imbalance detected: largest section has ", 
          round(ratio, 1), "x more responses than smallest."
        ))
      }
    }
    
    # Check for experience distribution
    if (nrow(exp_counts) > 0) {
      exp_pct <- exp_counts %>%
        mutate(pct = n / sum(n) * 100)
      
      if (any(exp_pct$pct > 60)) {
        dominant_exp <- exp_pct$experience[which.max(exp_pct$pct)]
        patterns <- c(patterns, paste0(
          "• Experience distribution skewed: ", 
          round(max(exp_pct$pct), 1), "% of students have similar experience levels."
        ))
      }
    }
    
    if (length(patterns) > 0) {
      insights$notable_patterns <- paste(patterns, collapse = "<br>")
    } else {
      insights$notable_patterns <- "• No notable patterns detected in current data."
    }
    
    # Recommendations
    recommendations <- c()
    
    # Based on learning preference
    if (nrow(pref_counts) > 0) {
      in_person_pct <- sum(data$learning_preference == "In-person", na.rm = TRUE) / nrow(data) * 100
      online_pct <- sum(data$learning_preference == "Online", na.rm = TRUE) / nrow(data) * 100
      
      if (in_person_pct > 60) {
        recommendations <- c(recommendations, 
          "• Consider maintaining or increasing in-person components given strong preference.")
      } else if (online_pct > 60) {
        recommendations <- c(recommendations,
          "• Consider enhancing online learning resources given strong preference.")
      } else {
        recommendations <- c(recommendations,
          "• Maintain hybrid approach to accommodate diverse learning preferences.")
      }
    }
    
    # Based on experience level
    if (nrow(exp_counts) > 0) {
      no_exp_pct <- sum(data$prior_experience == "No experience at all", na.rm = TRUE) / nrow(data) * 100
      
      if (no_exp_pct > 30) {
        recommendations <- c(recommendations,
          "• Consider providing additional support for students with no prior programming experience.")
      }
    }
    
    if (length(recommendations) > 0) {
      insights$recommendations <- paste(recommendations, collapse = "<br>")
    } else {
      insights$recommendations <- "• Continue current approach based on positive feedback patterns."
    }
    
    return(insights)
  })
  
  # Render insights panel
  output$overview_insights_key_findings <- renderUI({
    insights <- overview_insights()
    HTML(insights$key_findings)
  })
  
  output$overview_insights_statistical_highlights <- renderUI({
    insights <- overview_insights()
    HTML(insights$statistical_highlights)
  })
  
  output$overview_insights_notable_patterns <- renderUI({
    insights <- overview_insights()
    HTML(insights$notable_patterns)
  })
  
  output$overview_insights_recommendations <- renderUI({
    insights <- overview_insights()
    HTML(insights$recommendations)
  })
  
  # Return reactive values for potential use in other modules
  return(list(
    filtered_data = filtered_data,
    rv = rv
  ))
}
