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
    output$response_distribution_plot <- renderPlot({
      render_response_distribution_plot(df(), free_text_questions, plot_data_dist)
    })
    
    # Generate response length plot
    output$response_length_plot <- renderPlot({
      render_response_length_plot(df(), free_text_questions, plot_data_length)
    })
    
    # Handle plot click events
    handle_dist_plot_click(input, current_question, current_responses, selected_row, df, plot_data_dist, session, free_text_questions)
    handle_length_plot_click(input, current_question, current_responses, selected_row, df, plot_data_length, session, free_text_questions)
    handle_bar_click(input, current_question, current_responses, selected_row, df, session, free_text_questions)
    handle_y_axis_click(input, current_question, current_responses, selected_row, df, session, free_text_questions)
    
    # Send plot data to JavaScript when requested
    send_plot_data_to_js(input, session, plot_data_dist, plot_data_length)
  }
}