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
    # Uses ggiraph for interactivity - click events are captured via input$response_distribution_plot_selected
    output$response_distribution_plot <- renderGirafe({
      plot <- get_response_distribution_plot(df(), free_text_questions)
      girafe(
        ggobj = plot,
          options = list(
            opts_selection(type = "none")
          )
      )
    })
    
    # Generate response length plot
    # Uses ggiraph for interactivity - click events are captured via input$response_length_plot_selected
    output$response_length_plot <- renderGirafe({
      plot <- get_response_length_plot(df(), free_text_questions)
      girafe(
        ggobj = plot,
          options = list(
            opts_selection(type = "none")
          )
      )
    })
    
    # Handle ggiraph click events for response distribution plot
    # When a bar is clicked, the data_id (question_id) is captured in input$response_distribution_plot_selected
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
    
    # Handle ggiraph click events for response length plot
    # When a bar is clicked, the data_id (question_id) is captured in input$response_length_plot_selected
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