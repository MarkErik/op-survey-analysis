# CPSC Experience Survey Explorer - Shiny Application
# Main entry point for the survey data analysis application
#
# @author Course Instructor
# @version 2.0.0

# Load required packages
library(shiny)
library(bslib)

# Source global configuration and helper functions
source("global.R", local = TRUE)

# Source all component files -----------------------------------------------

# UI Components
source("R/ui_components.R", local = TRUE)
source("R/ui_home_tab.R", local = TRUE)
source("R/ui_responses_tab.R", local = TRUE)
source("R/ui_statistics_tab.R", local = TRUE)
source("R/ui_insights_tab.R", local = TRUE)

# Server Logic
source("R/server_home.R", local = TRUE)
source("R/server_responses.R", local = TRUE)
source("R/server_statistics.R", local = TRUE)
source("R/server_insights.R", local = TRUE)

# Utility Functions
source("R/utils_data_processing.R", local = TRUE)
source("R/utils_visualization.R", local = TRUE)
source("R/utils_statistics.R", local = TRUE)

# ==============================================================================
# UI DEFINITION
# ==============================================================================

#' Main User Interface
#'
#' Defines the layout and appearance of the Shiny application
#'
#' @param request Request object (automatically passed by Shiny)
#' @return UI definition
ui <- function(request) {
  bslib::page_navbar(
    title = APP_CONFIG$title,
    theme = get_app_theme(),

    # Navigation sidebar
    nav_panel(
      title = "Navigation",
      class = "nav-custom",
      value = "nav",
      ui_navigation_sidebar()
    ),

    # Home Tab
    nav_panel(
      title = "Home",
      icon = icon("home"),
      value = "home",
      ui_home_tab()
    ),

    # Question Responses Tab
    nav_panel(
      title = "Question Responses",
      icon = icon("comments"),
      value = "responses",
      ui_responses_tab()
    ),

    # Statistics Tab
    nav_panel(
      title = "Statistics",
      icon = icon("chart-bar"),
      value = "statistics",
      ui_statistics_tab()
    ),

    # Insights Tab
    nav_panel(
      title = "Insights",
      icon = icon("lightbulb"),
      value = "insights",
      ui_insights_tab()
    ),

    # Custom CSS
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      ),
      tags$style(HTML("
        .nav-custom {
          display: none;
        }
        .main-content {
          padding: 20px;
        }
        .card-custom {
          margin-bottom: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .section-filter {
          background-color: #f8f9fa;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 15px;
        }
        .question-button {
          margin: 5px;
          white-space: normal;
        }
        .stat-value {
          font-size: 24px;
          font-weight: bold;
          color: #2c3e50;
        }
        .stat-label {
          font-size: 12px;
          color: #7f8c8d;
        }
        .viz-container {
          background: white;
          padding: 15px;
          border-radius: 8px;
        }
      "))
    ),

    # Footer
    footer = tags$div(
      class = "app-footer",
      style = "text-align: center; padding: 15px; color: #7f8c8d;",
      HTML(paste0(
        "<strong>", APP_CONFIG$title, "</strong> v", APP_CONFIG$version, "<br>",
        "CPSC Course Experience Survey Analysis Tool"
      ))
    )
  )
}

# ==============================================================================
# SERVER DEFINITION
# ==============================================================================

#' Main Server Function
#'
#' Contains all reactive logic for the Shiny application
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
server <- function(input, output, session) {

  # Reactive values for shared state
  rv <- reactiveValues(
    selected_section = NULL,
    current_data = SURVEY_DATA
  )

  # Section filter observer
  observeEvent(input$section_filter, {
    rv$selected_section <- input$section_filter
    rv$current_data <- filter_by_section(SURVEY_DATA, input$section_filter)
  })

  # Reset filter observer
  observeEvent(input$reset_filter, {
    rv$selected_section <- NULL
    rv$current_data <- SURVEY_DATA
    updateSelectInput(session, "section_filter", selected = "")
  })

  # Call tab server functions
  server_home(input, output, session, rv)
  server_responses(input, output, session, rv)
  server_statistics(input, output, session, rv)
  server_insights(input, output, session, rv)
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

# Launch the Shiny application
shinyApp(ui = ui, server = server)
