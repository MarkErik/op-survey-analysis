# =============================================================================
# HOME_TAB_UI.R - Home Tab UI Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# UI module for Home tab - defines all visualization outputs

homeTabUI <- function(id) {
  ns <- NS(id)
  
  htmltools::tags$div(class = "home-tab-module",
    # Response Overview Section
    htmltools::tags$div(class = "response-overview-section", role = "region", `aria-label` = "Response overview",
      htmltools::tags$div(class = "overview-row",
        # Total Responses Counter
        htmltools::tags$div(class = "total-responses-card",
          htmltools::tags$h3("Total Responses"),
          htmltools::tags$div(id = ns("totalResponsesCount"), class = "response-count", "0")
        ),
        # Section Breakdown Chart (Interactive)
        htmltools::tags$div(class = "section-chart-container",
          htmltools::tags$h4("Responses per Section"),
          ggiraph::girafeOutput(ns("sectionBreakdownChart"), height = "250px")
        )
      ),
      # Section Filter Display
      htmltools::tags$div(class = "section-filter-display", role = "region", `aria-label` = "Section filter status",
        htmltools::tags$span(class = "filter-label", "Selected Section: "),
        htmltools::tags$span(id = ns("selectedSectionName"), class = "section-name", "(All Sections)"),
        htmltools::tags$button(id = ns("resetFilterBtn"), class = "btn-reset", "Reset Filter", `aria-label` = "Reset section filter")
      )
    ),
    
    htmltools::tags$hr(),
    
    # Visualization Grid - 6 Charts
    htmltools::tags$div(class = "visualization-grid", role = "region", `aria-label` = "Overview visualizations",
      # 1. Learning Preference Distribution
      htmltools::tags$div(class = "visualization-card",
        htmltools::tags$h4(class = "viz-title", "Learning Preference Distribution"),
        htmltools::tags$div(class = "viz-plot-container", ggiraph::girafeOutput(ns("learningPreferenceChart"), height = "200px"))
      ),
      # 2. Prior Programming Experience
      htmltools::tags$div(class = "visualization-card",
        htmltools::tags$h4(class = "viz-title", "Prior Programming Experience"),
        htmltools::tags$div(class = "viz-plot-container", ggiraph::girafeOutput(ns("programmingExperienceChart"), height = "200px"))
      ),
      # 3. Course Satisfaction Overview
      htmltools::tags$div(class = "visualization-card",
        htmltools::tags$h4(class = "viz-title", "Course Satisfaction Overview"),
        htmltools::tags$div(class = "viz-plot-container", ggiraph::girafeOutput(ns("courseSatisfactionChart"), height = "200px"))
      ),
      # 4. Discord Engagement Metrics
      htmltools::tags$div(class = "visualization-card",
        htmltools::tags$h4(class = "viz-title", "Discord Engagement Metrics"),
        htmltools::tags$div(class = "viz-plot-container", ggiraph::girafeOutput(ns("discordEngagementChart"), height = "200px"))
      ),
      # 5. Most Valuable Learning Methods
      htmltools::tags$div(class = "visualization-card",
        htmltools::tags$h4(class = "viz-title", "Most Valuable Learning Methods"),
        htmltools::tags$div(class = "viz-plot-container", ggiraph::girafeOutput(ns("learningMethodsChart"), height = "200px"))
      ),
      # 6. Community Connection Scores
      htmltools::tags$div(class = "visualization-card",
        htmltools::tags$h4(class = "viz-title", "Community Connection Scores"),
        htmltools::tags$div(class = "viz-plot-container", ggiraph::girafeOutput(ns("communityConnectionChart"), height = "200px"))
      )
    )
  )
}