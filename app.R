# =============================================================================
# CPSC Experience Survey Explorer
# Main application file integrating all modules
# =============================================================================

# =============================================================================
# Library Imports
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(tidyverse)
library(ggiraph)

# =============================================================================
# Source Configuration Files
# =============================================================================

source("R/global.R")
source("R/app_config.R")
source("R/error_handling.R")

# =============================================================================
# Source Module Files
# =============================================================================

source("R/module_data.R")
source("R/module_filters.R")
source("R/module_home.R")
source("R/module_responses.R")
source("R/module_statistics.R")
source("R/module_insights.R")

# =============================================================================
# Application UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = APP_TITLE,
    titleWidth = "300px"
  ),
  
  # Sidebar
  dashboardSidebar(
    width = "300px",
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Question Responses", tabName = "responses", icon = icon("question")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Insights", tabName = "insights", icon = icon("lightbulb"))
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),
    
    # Global loading indicator
    div(
      id = "global_loading",
      style = "display: none;",
      div(
        class = "loading-overlay",
        div(
          class = "loading-spinner",
          icon("spinner", spin = TRUE, lib = "font-awesome"),
          span("Loading data...")
        )
      )
    ),
    
    # Global error notification
    div(
      id = "global_error",
      style = "display: none;",
      div(
        class = "error-notification",
        icon("exclamation-triangle", lib = "font-awesome"),
        span("An error occurred. Please refresh the page.")
      )
    ),
    
    # Tab content
    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          column(12,
            div(
              class = "home-content",
              homeUI("home")
            )
          )
        )
      ),
      
      # Question Responses Tab
      tabItem(
        tabName = "responses",
        fluidRow(
          column(12,
            div(
              class = "responses-content",
              responsesUI("responses")
            )
          )
        )
      ),
      
      # Statistics Tab
      tabItem(
        tabName = "statistics",
        fluidRow(
          column(12,
            div(
              class = "statistics-content",
              statisticsUI("statistics")
            )
          )
        )
      ),
      
      # Insights Tab
      tabItem(
        tabName = "insights",
        fluidRow(
          column(12,
            div(
              class = "insights-content",
              insightsUI("insights")
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# Application Server
# =============================================================================

server <- function(input, output, session) {
  
  # =============================================================================
  # Global Error Boundary
  # =============================================================================

  observe({
    tryCatch({
      # Check data file status
      data_status <- check_data_file(DATA_FILE_PATH)
      
      if (data_status$status != "ok") {
        # Show error notification
        show_error_notification(data_status$error_type)
        shinyjs::show("global_error")
        return()
      }
      
      # Hide error notification
      shinyjs::hide("global_error")
      
    }, error = function(e) {
      # Log and show error
      log_error("global_error_boundary", conditionMessage(e))
      show_error_notification("unknown")
      shinyjs::show("global_error")
    })
  })
  
  # =============================================================================
  # Initialize Data Module
  # =============================================================================

  data_server <- dataServer("data")
  
  # =============================================================================
  # Initialize Filter Module
  # =============================================================================

  filter_server <- filterServer("filter", data_server = data_server)
  
  # =============================================================================
  # Initialize Home Tab Module
  # =============================================================================

  home_server <- homeServer("home", data_server = data_server, filter_server = filter_server)
  
  # =============================================================================
  # Initialize Question Responses Tab Module
  # =============================================================================

  responses_server <- responsesServer("responses", data_server = data_server, filter_server = filter_server)
  
  # =============================================================================
  # Initialize Statistics Tab Module
  # =============================================================================

  statistics_server <- statisticsServer("statistics", data_server = data_server, filter_server = filter_server)
  
  # =============================================================================
  # Initialize Insights Tab Module
  # =============================================================================

  insights_server <- insightsServer("insights", data_server = data_server, filter_server = filter_server)
  
  # =============================================================================
  # Section Filter Propagation
  # =============================================================================

  observeEvent(input$selected_section, {
    tryCatch({
      # Update filter server
      filter_server$updateSelectedSection(input$selected_section)
      
      # Update available sections from data server
      sections <- data_server$getSectionsReactive()
      filter_server$updateAvailableSections(sections)
      
    }, error = function(e) {
      # Log error but don't show notification
      log_error("filter_propagation", conditionMessage(e))
    })
  })
  
  # =============================================================================
  # Data Loading State Management
  # =============================================================================

  observe({
    isLoading <- data_server$isLoading()
    
    if (isLoading) {
      shinyjs::show("global_loading")
    } else {
      shinyjs::hide("global_loading")
    }
  })
  
  # =============================================================================
  # Session Management
  # =============================================================================

  session$onSessionEnded(function() {
    # Save filter state on session end
    filter_server$saveFilterState()
  })
  
  # =============================================================================
  # Initialize Filter State on App Start
  # =============================================================================

  observe({
    tryCatch({
      # Load saved filter state
      filter_server$loadFilterState()
      
      # Update available sections
      sections <- data_server$getSectionsReactive()
      filter_server$updateAvailableSections(sections)
      
    }, error = function(e) {
      # Log error but don't show notification
      log_error("filter_state_init", conditionMessage(e))
    })
  })
}

# =============================================================================
# Create Shiny Application
# =============================================================================

shinyApp(ui = ui, server = server)
