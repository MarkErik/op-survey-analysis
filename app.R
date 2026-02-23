# =============================================================================
# app.R - Main Entry Point for Shiny Survey Analysis Application
# =============================================================================
# Description: This file serves as the application entry point, orchestrating
# the loading of dependencies, sourcing core modules, and launching the Shiny
# application with proper error handling and startup validation.
# =============================================================================

# -----------------------------------------------------------------------------
# Step 1: Source Core Files (in dependency order)
# -----------------------------------------------------------------------------
# global.R must be sourced first - contains configuration, data loading,
# and utility functions required by ui.R and server.R
source("global.R")

# ui.R defines the user interface components and layout
source("ui.R")

# server.R contains the reactive server logic
source("server.R")

# -----------------------------------------------------------------------------
# Step 2: Startup Validation
# -----------------------------------------------------------------------------
# Validate that survey data exists and has required columns before launching
validate_startup <- function() {
  required_columns <- c("question", "response", "timestamp")
  
  if (!exists("survey_data") || is.null(survey_data)) {
    return(list(valid = FALSE, message = "Survey data not loaded"))
  }
  
  missing_cols <- setdiff(required_columns, names(survey_data))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  return(list(valid = TRUE, message = "Validation passed"))
}

# -----------------------------------------------------------------------------
# Step 3: Application Launch with Error Handling
# -----------------------------------------------------------------------------
tryCatch({
  # Perform startup validation
  validation_result <- validate_startup()
  
  if (!validation_result$valid) {
    warning(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
                   "Startup validation failed: ", validation_result$message))
  }
  
  # Log successful startup
  message(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
                 "Launching Shiny application..."))
  
  # Launch the Shiny application
  shiny::shinyApp(ui = ui, server = server)
  
}, error = function(e) {
  # Log error with timestamp
  error_msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
                      "Startup error: ", conditionMessage(e))
  message(error_msg)
  
  # Display user-friendly error UI
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::h2("Application Error"),
      shiny::div(
        style = "color: red; padding: 20px;",
        shiny::p("Unable to load survey data. Please ensure the data file exists and is properly formatted."),
        shiny::p(shiny::strong("Error details: "), error_msg)
      )
    ),
    server = function(input, output, session) {}
  )
})
