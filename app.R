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
    
    # Global loading indicator
    div(
      id = "global_loading",
      shinyjs::hidden(
        div(
          class = "loading-overlay",
          div(
            class = "loading-spinner",
            icon("spinner", spin = TRUE, lib = "font-awesome"),
            span("Loading data...")
          )
        )
      )
    ),
    
    # Global error notification
    div(
      id = "global_error",
      shinyjs::hidden(
        div(
          class = "error-notification",
          icon("exclamation-triangle", lib = "font-awesome"),
          span("An error occurred. Please refresh the page."),
          shinyjs::onclick("global_error", shinyjs::hide("global_error"))
        )
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
  # Global Error Handling
  # =============================================================================
  
  observe({
    tryCatch({
      # Check if data file exists
      if (!file.exists(DATA_FILE_PATH)) {
        shinyjs::show("global_error")
        return()
      }
      
      # Check if data is empty
      data <- load_survey_data(DATA_FILE_PATH)
      if (nrow(data) == 0) {
        shinyjs::show("global_error")
        return()
      }
      
      # Hide error notification
      shinyjs::hide("global_error")
      
    }, error = function(e) {
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
      # Silently handle errors
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
      # Silently handle errors
    })
  })
}

# =============================================================================
# Create Shiny Application
# =============================================================================

shinyApp(ui = ui, server = server)
