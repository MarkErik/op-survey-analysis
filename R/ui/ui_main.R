# Main UI Component

# Source all UI components
source("R/ui/ui_header.R", local = TRUE)
source("R/ui/ui_components.R", local = TRUE)
source("R/ui/ui_overview_tab.R", local = TRUE)
source("R/ui/ui_course_content_tab.R", local = TRUE)
source("R/ui/ui_learning_elements_tab.R", local = TRUE)
source("R/ui/ui_community_tab.R", local = TRUE)
source("R/ui/ui_free_text_tab.R", local = TRUE)
source("R/utils/utility_functions.R", local = TRUE)

# Create the main UI
create_main_ui <- function(free_text_questions) {
  fluidPage(
    # Include custom CSS with dynamic cache busting
    # Each CSS file gets a version based on its modification time
    tags$head(
      # Load CSS files in correct order (variables first, then others)
      create_css_link("css/themes/variables.css"),
      create_css_link("css/themes/global.css"),
      create_css_link("css/layout/layout.css"),
      create_css_link("css/layout/responsive.css"),
      create_css_link("css/components/components.css"),
      create_css_link("css/utilities/utilities.css")
    ),
    
    # Header
    create_app_header(),
    
    # Main content area
    fluidRow(
      column(12,
        tabsetPanel(
          id = "tabset",
          create_overview_tab(),
          create_course_content_tab(),
          create_learning_elements_tab(),
          create_community_tab(),
          create_free_text_tab(free_text_questions)
        )
      )
    )
  )
}