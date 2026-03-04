library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(tidyverse)
library(ggiraph)

source("R/global.R")
source("R/app_config.R")
source("R/error_handling.R")

source("R/module_data.R")
source("R/module_filters.R")
source("R/module_home.R")
source("R/module_responses.R")
source("R/module_statistics.R")
source("R/module_insights.R")

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = APP_TITLE,
    titleWidth = "300px"
  ),
  
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
    
    div(
      id = "global_error",
      style = "display: none;",
      div(
        class = "error-notification",
        icon("exclamation-triangle", lib = "font-awesome"),
        span("An error occurred. Please refresh the page.")
      )
    ),
    
    tabItems(
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

server <- function(input, output, session) {

  observe({
    tryCatch({
      data_status <- check_data_file(DATA_FILE_PATH)
      
      if (data_status$status != "ok") {
        show_error_notification(data_status$error_type)
        shinyjs::show("global_error")
        return()
      }
      
      shinyjs::hide("global_error")
      
    }, error = function(e) {
      log_error("global_error_boundary", conditionMessage(e))
      show_error_notification("unknown")
      shinyjs::show("global_error")
    })
  })
  
  data_server <- dataServer("data")

  filter_server <- filterServer("filter", data_server = data_server)

  home_server <- homeServer("home", data_server = data_server, filter_server = filter_server)

  responses_server <- responsesServer("responses", data_server = data_server, filter_server = filter_server)

  statistics_server <- statisticsServer("statistics", data_server = data_server, filter_server = filter_server)

  insights_server <- insightsServer("insights", data_server = data_server, filter_server = filter_server)

  observeEvent(input$selected_section, {
    tryCatch({
      filter_server$updateSelectedSection(input$selected_section)
      
      sections <- data_server$getSectionsReactive()
      filter_server$updateAvailableSections(sections)
      
    }, error = function(e) {
      log_error("filter_propagation", conditionMessage(e))
    })
  })

  observe({
    isLoading <- data_server$isLoading()
    
    if (isLoading) {
      shinyjs::show("global_loading")
    } else {
      shinyjs::hide("global_loading")
    }
  })
  

  session$onSessionEnded(function() {
    filter_server$saveFilterState()
  })
  
  observe({
    tryCatch({
      filter_server$loadFilterState()
      
      sections <- data_server$getSectionsReactive()
      filter_server$updateAvailableSections(sections)
      
    }, error = function(e) {
      log_error("filter_state_init", conditionMessage(e))
    })
  })
}

shinyApp(ui = ui, server = server)
