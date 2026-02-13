# Main Server Component

# Source all server modules
source("R/server/server_statistics.R", local = TRUE)
source("R/server/server_overview.R", local = TRUE)
source("R/server/server_course_content.R", local = TRUE)
source("R/server/server_learning_elements.R", local = TRUE)
source("R/server/server_community.R", local = TRUE)
source("R/server/server_free_text.R", local = TRUE)
source("R/server/server_insights.R", local = TRUE)

# Source data processing and utility functions
source("R/data/data_processing.R", local = TRUE)
source("R/utils/utility_functions.R", local = TRUE)

# Source all visualization modules
source("R/visualization/plot_overview.R", local = TRUE)
source("R/visualization/plot_course_content.R", local = TRUE)
source("R/visualization/plot_learning_elements.R", local = TRUE)
source("R/visualization/plot_community.R", local = TRUE)
source("R/visualization/plot_free_text.R", local = TRUE)

# Create the main server function
create_main_server <- function(free_text_questions) {
  function(input, output, session) {
    # Load data
    df <- reactive({
      load_data()
    })
    
    # Setup Overview tab
    setup_overview_tab(input, output, session, df)
    
    # Setup Course Content tab
    setup_course_content_tab(input, output, session, df)
    
    # Setup Learning Elements tab
    setup_learning_elements_tab(input, output, session, df)
    
    # Setup Community & Belonging tab
    setup_community_tab(input, output, session, df)
    
    # Setup Free Text tab
    setup_free_text_tab(input, output, session, df, free_text_questions)
  }
}
