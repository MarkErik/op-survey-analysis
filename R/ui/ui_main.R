# Main UI Component

# Source all UI components
source("R/ui/ui_header.R", local = TRUE)
source("R/ui/ui_sidebar.R", local = TRUE)
source("R/ui/ui_home_tab.R", local = TRUE)
source("R/ui/ui_question_responses_tab.R", local = TRUE)
source("R/ui/ui_participant_profile_tab.R", local = TRUE)

# Create the main UI
create_main_ui <- function(free_text_questions) {
  # CSS version for cache busting - update this when CSS changes
  css_version <- "1.0.0"
  
  fluidPage(
    # Include custom CSS with cache busting
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("css/style.css?v=", css_version)
      )
    ),
    
    # Include custom JavaScript for chart click handling
    includeScript("www/js/chart_click_handler.js"),
    
    # Header
    create_app_header(),
    
    # Main content area
    fluidRow(
      column(3,
        create_sidebar_panel(free_text_questions)
      ),
      column(9,
        tabsetPanel(
          id = "tabset",
          create_home_tab(),
          create_question_responses_tab(),
          create_participant_profile_tab()
        )
      )
    )
  )
}