# =============================================================================
# SERVER.R - Main Server Entry Point
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================
# File Description:
#   Main server file that orchestrates all tab modules and manages shared state
#   through reactive data flow. Handles section filtering and error management.
#
# Module Dependencies:
#   - home_tab_server.R (homeTabServer)
#   - question_responses_server.R (questionResponsesServer)
#   - statistics_tab_server.R (statisticsTabServer)
#   - insights_tab_server.R (insightsTabServer)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. SOURCE MODULE FILES
# -----------------------------------------------------------------------------

# Source global configuration and data loading
source("global.R")

# Source tab server modules
source("home_tab_server.R")
source("question_responses_server.R")
source("statistics_tab_server.R")
source("insights_tab_server.R")

# -----------------------------------------------------------------------------
# 2. MAIN SERVER FUNCTION
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Initialize shared reactive values
  rv <- shiny::reactiveValues(
    filteredData = NULL,      # Section-filtered data shared across modules
    selectedSection = NULL    # Currently selected section (NULL = all)
  )
  
  # Load initial data with progress indicator
  shiny::observe({
    shiny::withProgress(message = "Loading survey data...", value = 0, {
      tryCatch({
        shiny::incProgress(0.5, detail = "Reading CSV file")
        raw_data <- load_survey_data()
        shiny::incProgress(0.5, detail = "Data loaded successfully")
        rv$filteredData <- raw_data
        rv$selectedSection <- NULL
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to load data:", e$message),
          type = "error",
          duration = 10
        )
        log_error("Data loading failed", e)
      })
    })
  })
  
  # ---------------------------------------------------------------------------
  # 3. REACTIVE DATA FLOW - Section Filtering
  # ---------------------------------------------------------------------------
  
  # Listen for section filter changes from Home tab
  shiny::observeEvent(input$section_filter, {
    req(rv$filteredData)
    newSection <- input$section_filter
    
    shiny::withProgress(message = "Filtering data...", value = 0.5, {
      if (is.null(newSection) || newSection == "" || newSection == "All") {
        # Reset to all data
        rv$selectedSection <- NULL
        rv$filteredData <- load_survey_data()
      } else {
        # Filter by selected section
        rv$selectedSection <- newSection
        all_data <- load_survey_data()
        rv$filteredData <- all_data[all_data$section == newSection, ]
      }
      shiny::showNotification(
        if(is.null(rv$selectedSection)) "Filter reset - showing all sections" 
        else paste("Showing data for:", rv$selectedSection),
        type = "message",
        duration = 3
      )
    })
  })
  
  # Handle reset filter button
  shiny::observeEvent(input$reset_filter, {
    rv$selectedSection <- NULL
    rv$filteredData <- load_survey_data()
    shiny::updateSelectInput(session, "section_filter", selected = "All")
    shiny::showNotification("Filter reset - showing all sections", type = "message", duration = 3)
  })
  
  # Export filteredData reactive for child modules
  getFilteredData <- shiny::reactive({
    req(rv$filteredData)
    rv$filteredData
  })
  
  # ---------------------------------------------------------------------------
  # 4. MODULE INITIALIZATION
  # ---------------------------------------------------------------------------
  
  # Initialize Home Tab module
  tryCatch({
    homeTabServer("home", data = getFilteredData)
  }, error = function(e) {
    log_error("Home tab module initialization failed", e)
    shiny::showNotification("Failed to initialize Home tab", type = "error", duration = 10)
  })
  
  # Initialize Question Responses Tab module
  tryCatch({
    questionResponsesServer("questionResponses", filteredData = getFilteredData)
  }, error = function(e) {
    log_error("Question Responses tab module initialization failed", e)
    shiny::showNotification("Failed to initialize Question Responses tab", type = "error", duration = 10)
  })
  
  # Initialize Statistics Tab module
  tryCatch({
    statisticsTabServer("statistics", filteredData = getFilteredData)
  }, error = function(e) {
    log_error("Statistics tab module initialization failed", e)
    shiny::showNotification("Failed to initialize Statistics tab", type = "error", duration = 10)
  })
  
  # Initialize Insights Tab module
  tryCatch({
    insightsTabServer("insights", filteredData = getFilteredData)
  }, error = function(e) {
    log_error("Insights tab module initialization failed", e)
    shiny::showNotification("Failed to initialize Insights tab", type = "error", duration = 10)
  })
}

# -----------------------------------------------------------------------------
# 5. UTILITY FUNCTIONS
# -----------------------------------------------------------------------------

# Log errors with timestamp to console and file
log_error <- function(context, error_obj) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  error_msg <- paste0("[", timestamp, "] ", context, ": ", error_obj$message)
  message(error_msg)
  
  # Append to error log file if writable
  tryCatch({
    cat(error_msg, "\n", file = "error.log", append = TRUE)
  }, error = function(e) {
    # Silently fail if can't write to log
  })
}

# Export server function
moduleServer <- shiny::moduleServer