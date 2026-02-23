# =============================================================================
# STATISTICS_TAB_UI.R - Statistics Tab UI Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# UI module for Statistics tab - category selector, question buttons, and statistical displays

statisticsTabUI <- function(id) {
  ns <- NS(id)
  
  htmltools::tags$div(class = "statistics-tab", role = "region", `aria-label` = "Statistics analysis",
    # Category Selector Section
    htmltools::tags$div(class = "category-selector-section",
      htmltools::tags$h3("Select Question Category"),
      htmltools::tags$select(id = ns("categorySelector"), class = "form-control",
        htmltools::tags$option(value = "course_satisfaction", "Course Satisfaction (6 questions)"),
        htmltools::tags$option(value = "learning_methods", "Learning Methods (10 questions)"),
        htmltools::tags$option(value = "community_belonging", "Community & Belonging (5 questions)")
      )
    ),
    
    htmltools::tags$hr(),
    
    # Question Button Container
    htmltools::tags$div(class = "question-buttons-section",
      htmltools::tags$h3("Select a Question"),
      htmltools::tags$div(id = ns("questionButtonsContainer"), class = "question-buttons-row")
    ),
    
    htmltools::tags$hr(),
    
    # Statistical Summary Panel
    htmltools::tags$div(class = "statistics-summary-panel",
      htmltools::tags$h3("Descriptive Statistics"),
      htmltools::tags$div(class = "stats-grid",
        shinydashboard::valueBoxOutput(ns("statN"), width = 3),
        shinydashboard::valueBoxOutput(ns("statMean"), width = 3),
        shinydashboard::valueBoxOutput(ns("statMedian"), width = 3),
        shinydashboard::valueBoxOutput(ns("statMode"), width = 3),
        shinydashboard::valueBoxOutput(ns("statSD"), width = 3),
        shinydashboard::valueBoxOutput(ns("statSE"), width = 3),
        shinydashboard::valueBoxOutput(ns("statMin"), width = 3),
        shinydashboard::valueBoxOutput(ns("statMax"), width = 3),
        shinydashboard::valueBoxOutput(ns("statQ1"), width = 3),
        shinydashboard::valueBoxOutput(ns("statQ3"), width = 3),
        shinydashboard::valueBoxOutput(ns("statMissing"), width = 3)
      )
    ),
    
    htmltools::tags$hr(),
    
    # Distribution Histogram
    htmltools::tags$div(class = "distribution-section",
      htmltools::tags$h3("Response Distribution"),
      ggiraph::girafeOutput(ns("distributionHistogram"))
    ),
    
    htmltools::tags$hr(),
    
    # Section Comparison Toggle
    htmltools::tags$div(class = "section-comparison-section",
      htmltools::tags$h3("Section Comparison"),
      htmltools::tags$label(
        htmltools::tags$input(id = ns("compareSectionsToggle"), type = "checkbox"),
        "Compare Across Sections"
      ),
      htmltools::tags$div(id = ns("sectionComparisonContainer"), class = "section-comparison-charts")
    )
  )
}