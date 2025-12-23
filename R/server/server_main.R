# Main Server Component

# Source all server modules
source("R/server/server_responses.R", local = TRUE)
source("R/server/server_statistics.R", local = TRUE)
source("R/server/server_plots.R", local = TRUE)

# Source data processing and utility functions
source("R/data/data_processing.R", local = TRUE)
source("R/utils/utility_functions.R", local = TRUE)
source("R/visualization/plot_generation.R", local = TRUE)

# Create the main server function
create_main_server <- function(free_text_questions) {
  function(input, output, session) {
    # Load data
    df <- reactive({
      load_data()
    })
    
    # Current question state
    current_question <- reactiveVal(NULL)
    current_responses <- reactiveVal(NULL)
    selected_row <- reactiveVal(NULL)
    
    # Plot data for click detection
    plot_data_dist <- reactiveVal(NULL)
    plot_data_length <- reactiveVal(NULL)
    
    # Handle question button clicks
    observe({
      lapply(names(free_text_questions), function(question) {
        observeEvent(input[[paste0("btn_", question)]], {
          current_question(question)
          current_responses(get_responses_for_question(df(), question))
          selected_row(NULL)
          
          # Switch to Question Responses tab
          updateTabsetPanel(session, "tabset", selected = "Question Responses")
        })
      })
    })
    
    # Render responses table
    output$responses_table <- renderDT({
      render_responses_table(current_responses, free_text_questions, current_question)
    })
    
    # Handle row selection in responses table
    handle_row_selection(input, selected_row, session)
    
    # Render participant profile
    output$profile_ui <- renderUI({
      render_participant_profile(selected_row, current_responses, current_question, df(), free_text_questions)
    })
    
    # Update statistics on home page
    stats <- reactive({
      update_statistics(df(), free_text_questions)
    })
    
    output$total_responses <- renderText({
      stats()$total_responses
    })
    
    output$question_count <- renderText({
      stats()$question_count
    })
    
    output$avg_response_length <- renderText({
      stats()$avg_response_length
    })
    
    # Generate response distribution plot
    output$response_distribution_plot <- renderGirafe({
      plot_result <- render_response_distribution_plot(df(), free_text_questions, plot_data_dist)
      girafe(ggobj = plot_result$plot)
    })
    
    # Generate response length plot
    output$response_length_plot <- renderGirafe({
      plot_result <- render_response_length_plot(df(), free_text_questions, plot_data_length)
      girafe(ggobj = plot_result$plot)
    })
    
    # Handle ggiraph click events
    observeEvent(input$response_distribution_plot_selected, {
      selected_data <- input$response_distribution_plot_selected
      if (!is.null(selected_data) && length(selected_data) > 0) {
        question_key <- selected_data[1]  # Get the first selected question ID
        if (!is.null(question_key) && question_key %in% names(free_text_questions)) {
          current_question(question_key)
          current_responses(get_responses_for_question(df(), question_key))
          selected_row(NULL)
          
          # Switch to Question Responses tab
          updateTabsetPanel(session, "tabset", selected = "Question Responses")
        }
      }
    })
    
    observeEvent(input$response_length_plot_selected, {
      selected_data <- input$response_length_plot_selected
      if (!is.null(selected_data) && length(selected_data) > 0) {
        question_key <- selected_data[1]  # Get the first selected question ID
        if (!is.null(question_key) && question_key %in% names(free_text_questions)) {
          current_question(question_key)
          current_responses(get_responses_for_question(df(), question_key))
          selected_row(NULL)
          
          # Switch to Question Responses tab
          updateTabsetPanel(session, "tabset", selected = "Question Responses")
        }
      }
    })
    
  }
}