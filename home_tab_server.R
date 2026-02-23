# =============================================================================
# HOME_TAB_SERVER.R - Home Tab Server Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# Server module for Home tab - reactive logic and visualization rendering

homeTabServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive: Track selected section for filtering
    selectedSection <- shiny::reactiveVal(NULL)
    
    # Reactive: Filter data by selected section
    filteredData <- shiny::reactive({
      req(data())
      df <- data()
      if (!is.null(selectedSection()) && selectedSection() != "All") {
        df <- df[df$section == selectedSection(), ]
      }
      df
    })
    
    # Update total responses counter
    shiny::observe({
      count <- nrow(filteredData())
      shiny::updateTextInput(session, "totalResponsesCount", value = as.character(count))
      htmltools::runJavaScript(paste0("document.getElementById('", ns("totalResponsesCount"), "').textContent = '", count, "';"))
    })
    
    # Update selected section display
    shiny::observe({
      section <- selectedSection()
      sectionLabel <- if (is.null(section) || section == "All") "(All Sections)" else paste0("(", section, ")")
      htmltools::runJavaScript(paste0("document.getElementById('", ns("selectedSectionName"), "').textContent = '", sectionLabel, "';"))
    })
    
    # Handle section click for filtering
    shiny::observeEvent(input$sectionBreakdownChart_selected, {
      req(input$sectionBreakdownChart_selected)
      selected <- input$sectionBreakdownChart_selected
      if (length(selected) > 0) {
        # Get the section name from the clicked bar
        sectionName <- selected$id
        selectedSection(sectionName)
      }
    })
    
    # Handle reset filter button
    shiny::observeEvent(input$resetFilterBtn, {
      selectedSection("All")
    })
    
    # Helper: Get filter label for chart titles
    getFilterLabel <- function() {
      if (is.null(selectedSection()) || selectedSection() == "All") "(All Sections)" else paste0("(", selectedSection(), ")")
    }
    
    # 1. Section Breakdown Chart (Interactive)
    output$sectionBreakdownChart <- ggiraph::renderGirafe({
      req(data())
      tryCatch({
        df <- data()
        sectionCounts <- as.data.frame(table(df$section))
        names(sectionCounts) <- c("section", "count")
        sectionCounts <- sectionCounts[sectionCounts$section != "", ]
        
        p <- ggplot2::ggplot(sectionCounts, ggplot2::aes(x = section, y = count, fill = section)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Section: ", section, "\nResponses: ", count),
                         data_id = section),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Responses per Section", getFilterLabel()),
                        x = "Section", y = "Number of Responses") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        ggiraph::girafe(ggobj = p, options = list(ggiraph::opts_selection(type = "single")))
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading section data"))
      })
    })
    
    # 2. Learning Preference Distribution
    output$learningPreferenceChart <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        prefCounts <- as.data.frame(table(df$learning_preference))
        names(prefCounts) <- c("preference", "count")
        
        p <- ggplot2::ggplot(prefCounts, ggplot2::aes(x = preference, y = count, fill = preference)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Preference: ", preference, "\nCount: ", count)),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Learning Preference Distribution", getFilterLabel()),
                        x = "Preference", y = "Count") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading data"))
      })
    })
    
    # 3. Prior Programming Experience (Horizontal Bar)
    output$programmingExperienceChart <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        expCounts <- as.data.frame(table(df$prior_experience))
        names(expCounts) <- c("experience", "count")
        
        p <- ggplot2::ggplot(expCounts, ggplot2::aes(x = count, y = experience, fill = experience)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Experience: ", experience, "\nCount: ", count)),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Prior Programming Experience", getFilterLabel()),
                        x = "Count", y = "Experience Level") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading data"))
      })
    })
    
    # 4. Course Satisfaction Overview (Average Scores)
    output$courseSatisfactionChart <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        satCols <- grep("^satisfaction_", names(df), value = TRUE)
        satLabels <- c("Content Relevant", "Excited", "Feedback", "Apply Learning", "Ask Help", "Python Goals")
        
        satMeans <- sapply(satCols, function(col) mean(df[[col]], na.rm = TRUE))
        satDF <- data.frame(label = satLabels, score = satMeans)
        
        p <- ggplot2::ggplot(satDF, ggplot2::aes(x = score, y = label, fill = label)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Aspect: ", label, "\nAvg Score: ", round(score, 2))),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Course Satisfaction Overview", getFilterLabel()),
                        x = "Average Score (1-5)", y = "") +
          ggplot2::xlim(0, 5) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading data"))
      })
    })
    
    # 5. Discord Engagement Metrics
    output$discordEngagementChart <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        total <- nrow(df)
        
        discordMetrics <- data.frame(
          metric = c("Joined Discord", "Active on Discord", "Find Discord Useful"),
          percent = c(
            sum(df$discord_joined, na.rm = TRUE) / total * 100,
            sum(df$discord_active, na.rm = TRUE) / total * 100,
            sum(df$discord_useful, na.rm = TRUE) / total * 100
          )
        )
        
        p <- ggplot2::ggplot(discordMetrics, ggplot2::aes(x = percent, y = metric, fill = metric)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Metric: ", metric, "\nPercentage: ", round(percent, 1), "%")),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Discord Engagement Metrics", getFilterLabel()),
                        x = "Percentage (%)", y = "") +
          ggplot2::xlim(0, 100) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading data"))
      })
    })
    
    # 6. Most Valuable Learning Methods (Top 7)
    output$learningMethodsChart <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        methodCols <- grep("^method_", names(df), value = TRUE)
        methodLabels <- c("Pre-written Code", "Midterms", "TopHat Quizzes", "Slides", "Handouts",
                          "Coding on Own", "Live Coding", "Labs", "Ask Professor", "Assignments")
        
        methodMeans <- sapply(methodCols, function(col) mean(df[[col]], na.rm = TRUE))
        methodDF <- data.frame(label = methodLabels, score = methodMeans)
        methodDF <- methodDF[order(-methodDF$score), ][1:7, ]
        
        p <- ggplot2::ggplot(methodDF, ggplot2::aes(x = score, y = label, fill = label)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Method: ", label, "\nAvg Rating: ", round(score, 2))),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Most Valuable Learning Methods", getFilterLabel()),
                        x = "Average Rating (1-5)", y = "") +
          ggplot2::xlim(0, 5) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading data"))
      })
    })
    
    # 7. Community Connection Scores
    output$communityConnectionChart <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        commCols <- grep("^community_", names(df), value = TRUE)
        commLabels <- c("Comfort Speaking Up", "Part of Class", "Making Friends Important",
                        "Part of University", "Easy to Meet People")
        
        commMeans <- sapply(commCols, function(col) mean(df[[col]], na.rm = TRUE))
        commDF <- data.frame(label = commLabels, score = commMeans)
        
        p <- ggplot2::ggplot(commDF, ggplot2::aes(x = score, y = label, fill = label)) +
          ggplot2::geom_col() +
          ggplot2::geom_bar_interactive(
            ggplot2::aes(tooltip = paste0("Aspect: ", label, "\nAvg Score: ", round(score, 2))),
            width = 0.7
          ) +
          ggplot2::labs(title = paste("Community Connection Scores", getFilterLabel()),
                        x = "Average Score (1-5)", y = "") +
          ggplot2::xlim(0, 5) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "Error loading data"))
      })
    })
    
    # Return reactive values for use by other modules
    return(list(
      selectedSection = selectedSection,
      filteredData = filteredData
    ))
  })
}