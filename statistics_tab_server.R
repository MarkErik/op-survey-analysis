# =============================================================================
# STATISTICS_TAB_SERVER.R - Statistics Tab Server Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# Server module for Statistics tab - reactive logic and statistical analysis

statisticsTabServer <- function(id, filteredData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Question mappings by category
    categoryQuestions <- list(
      course_satisfaction = list(
        cols = 8:13,
        names = c("Content relevance", "Excitement", "Feedback satisfaction", 
                  "Apply learning", "Ask help", "Goal achievement"),
        prefixes = paste0("COL_COURSE_SATISFACTION_", 1:6)
      ),
      learning_methods = list(
        cols = 14:23,
        names = c("Pre-written code", "Midterms", "TopHat", "Slides", "Handouts",
                  "Coding own", "Live coding", "Labs", "Asking questions", "Assignments"),
        prefixes = paste0("COL_LEARNING_METHODS_", 1:10)
      ),
      community_belonging = list(
        cols = 27:31,
        names = c("Comfort speaking up", "Feeling part of class", "Making friends important",
                  "University community", "Meeting people"),
        prefixes = paste0("COL_COMMUNITY_", 1:5)
      )
    )
    
    # Reactive: Selected category
    selectedCategory <- shiny::reactive({
      input$categorySelector
    })
    
    # Reactive: Selected question
    selectedQuestion <- shiny::reactiveVal(NULL)
    
    # Reactive: Get question data for selected category
    categoryData <- shiny::reactive({
      req(selectedCategory())
      categoryQuestions[[selectedCategory()]]
    })
    
    # Render question buttons dynamically
    shiny::observe({
      req(categoryData())
      qData <- categoryData()
      buttons <- lapply(seq_along(qData$names), function(i) {
        htmltools::tags$button(
          id = ns(paste0("qBtn_", i)),
          class = "question-btn",
          `data-question` = i,
          htmltools::HTML(paste0(i, ". ", qData$names[i]))
        )
      })
      htmltools::runJavaScript(paste0(
        "document.getElementById('", ns("questionButtonsContainer"), "').innerHTML = '",
        htmltools::renderTags(htmltools::tags$div(class = "question-buttons-row", buttons))$html,
        "';"
      ))
    })
    
    # Handle question button clicks
    shiny::observe({
      req(categoryData())
      lapply(seq_along(categoryData()$names), function(i) {
        shiny::observeEvent(input[[paste0("qBtn_", i)]], {
          selectedQuestion(i)
        })
      })
    })
    
    # Reactive: Get selected question column
    selectedQuestionCol <- shiny::reactive({
      req(selectedQuestion())
      req(categoryData())
      categoryData()$cols[selectedQuestion()]
    })
    
    # Reactive: Get selected question values
    questionValues <- shiny::reactive({
      req(selectedQuestionCol())
      req(filteredData())
      tryCatch({
        df <- filteredData()
        colIdx <- selectedQuestionCol()
        values <- sapply(df[[colIdx]], function(x) {
          if (is.na(x) || x == "") return(NA_real_)
          num <- stringr::str_extract(as.character(x), "[1-5]")
          if (is.na(num)) return(NA_real_)
          as.numeric(num)
        })
        values
      }, error = function(e) {
        warning(paste("Error extracting values:", e$message))
        rep(NA_real_, nrow(filteredData()))
      })
    })
    
    # Helper: Calculate mode
    calcMode <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA_real_)
      tbl <- table(x)
      as.numeric(names(tbl)[which.max(tbl)])
    }
    
    # Reactive: Calculate descriptive statistics
    descriptiveStats <- shiny::reactive({
      req(questionValues())
      tryCatch({
        vals <- questionValues()
        validVals <- vals[!is.na(vals)]
        n <- length(validVals)
        if (n == 0) {
          return(list(N = 0, Mean = NA, Median = NA, Mode = NA, SD = NA, SE = NA,
                      Min = NA, Max = NA, Q1 = NA, Q3 = NA, Missing = sum(is.na(vals))))
        }
        list(
          N = n,
          Mean = round(mean(validVals), 2),
          Median = round(median(validVals), 2),
          Mode = calcMode(validVals),
          SD = round(sd(validVals), 2),
          SE = round(sd(validVals) / sqrt(n), 2),
          Min = min(validVals),
          Max = max(validVals),
          Q1 = round(quantile(validVals, 0.25), 2),
          Q3 = round(quantile(validVals, 0.75), 2),
          Missing = sum(is.na(vals))
        )
      }, error = function(e) {
        warning(paste("Error calculating stats:", e$message))
        NULL
      })
    })
    
    # Render statistical valueBoxes
    shiny::observe({
      stats <- descriptiveStats()
      if (is.null(stats)) return()
      
      updateValueBox(session, ns("statN"), "N", as.character(stats$N))
      updateValueBox(session, ns("statMean"), "Mean", as.character(stats$Mean))
      updateValueBox(session, ns("statMedian"), "Median", as.character(stats$Median))
      updateValueBox(session, ns("statMode"), "Mode", as.character(stats$Mode))
      updateValueBox(session, ns("statSD"), "SD", as.character(stats$SD))
      updateValueBox(session, ns("statSE"), "SE", as.character(stats$SE))
      updateValueBox(session, ns("statMin"), "Min", as.character(stats$Min))
      updateValueBox(session, ns("statMax"), "Max", as.character(stats$Max))
      updateValueBox(session, ns("statQ1"), "Q1", as.character(stats$Q1))
      updateValueBox(session, ns("statQ3"), "Q3", as.character(stats$Q3))
      updateValueBox(session, ns("statMissing"), "Missing", as.character(stats$Missing))
    })
    
    # Render distribution histogram
    output$distributionHistogram <- ggiraph::renderGirafe({
      req(questionValues())
      tryCatch({
        vals <- questionValues()
        validVals <- vals[!is.na(vals)]
        if (length(validVals) == 0) {
          return(ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "No data available")))
        }
        
        df <- data.frame(value = validVals)
        df$value <- factor(df$value, levels = 1:5)
        
        counts <- table(df$value)
        percentages <- round(prop.table(counts) * 100, 1)
        labels <- paste0(counts, "\n(", percentages, "%)")
        
        colors <- c("#d73027", "#fc8d59", "#fee08b", "#91bfdb", "#4575b4")
        
        p <- ggplot2::ggplot(df, ggplot2::aes(x = value, fill = value)) +
          ggplot2::geom_bar(fill = colors, color = "white") +
          ggplot2::geom_text(ggplot2::aes(label = labels, y = ..count..), 
                            stat = "count", vjust = -0.5, size = 4) +
          ggplot2::scale_x_discrete(labels = c("1 - Strongly Disagree", "2 - Disagree", 
                                               "3 - Neutral", "4 - Agree", "5 - Strongly Agree")) +
          ggplot2::labs(title = paste(categoryData()$names[selectedQuestion()], "- Response Distribution"),
                       x = "Response", y = "Count") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = paste("Error:", e$message)))
      })
    })
    
    # Section comparison charts
    output$sectionComparisonContainer <- ggiraph::renderGirafe({
      req(input$compareSectionsToggle)
      req(questionValues())
      req(filteredData())
      
      if (!input$compareSectionsToggle) {
        return(ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "")))
      }
      
      tryCatch({
        df <- filteredData()
        colIdx <- selectedQuestionCol()
        
        df$section <- df$section
        df$value <- sapply(df[[colIdx]], function(x) {
          if (is.na(x) || x == "") return(NA_real_)
          num <- stringr::str_extract(as.character(x), "[1-5]")
          if (is.na(num)) return(NA_real_)
          as.numeric(num)
        })
        
        df <- df[!is.na(df$value) & df$section != "", ]
        if (nrow(df) == 0) {
          return(ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = "No section data available")))
        }
        
        colors <- c("#d73027", "#fc8d59", "#fee08b", "#91bfdb", "#4575b4")
        
        p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(value, levels = 1:5), fill = section)) +
          ggplot2::geom_bar(position = "dodge", color = "white") +
          ggplot2::scale_fill_brewer(palette = "Set1") +
          ggplot2::scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
          ggplot2::labs(title = "Response Distribution by Section",
                       x = "Response", y = "Count") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = paste("Error:", e$message)))
      })
    })
  })
}

# Helper function to update valueBox content
updateValueBox <- function(session, outputId, title, value) {
  htmltools::runJavaScript(paste0(
    "var box = document.querySelector('#", outputId, " .value-box');",
    "if (box) { box.innerHTML = '<div class=\"inner\"><small class=\"value-box-title\">", title, 
    "</small><div class=\"value-box-value\">", value, "</div></div>'; }"
  ))
}