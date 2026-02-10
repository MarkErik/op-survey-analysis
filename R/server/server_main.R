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
    selected_section <- reactiveVal(NULL)
    
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
    
    # Handle section selection from section breakdown plot
    observeEvent(input$section_breakdown_plot_selected, {
      if (!is.null(input$section_breakdown_plot_selected)) {
        selected_section(input$section_breakdown_plot_selected)
      } else {
        selected_section(NULL)
      }
    })
    
    # Handle reset section filter button
    observeEvent(input$reset_section_filter, {
      selected_section(NULL)
    })
    
    # Create filtered data frame based on selected section
    filtered_df <- reactive({
      data <- df()
      if (!is.null(selected_section())) {
        data[data$section == selected_section(), ]
      } else {
        data
      }
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
      update_statistics(filtered_df(), free_text_questions)
    })
    
    output$total_responses <- renderText({
      stats()$total_responses
    })
    
    # Display selected section
    output$selected_section_display <- renderText({
      if (!is.null(selected_section())) {
        selected_section()
      } else {
        "All Sections"
      }
    })
    
    # Render reset filter button with dynamic class
    output$reset_section_filter_ui <- renderUI({
      btn_class <- if (!is.null(selected_section())) {
        "btn-reset btn-reset-active"
      } else {
        "btn-reset"
      }
      actionButton("reset_section_filter", "Reset Filter", class = btn_class)
    })
    
    # Generate learning preference plot
    output$learning_preference_plot <- renderGirafe({
      plot <- get_learning_preference_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
    # Generate prior experience plot
    output$prior_experience_plot <- renderGirafe({
      plot <- get_prior_experience_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
    # Generate section breakdown pie chart
    output$section_breakdown_plot <- renderGirafe({
      plot <- get_section_breakdown_plot(df())
      girafe(
        ggobj = plot,
        options = list(
          opts_selection(type = "single")
        )
      )
    })
    
    # Generate course satisfaction plot
    output$course_satisfaction_plot <- renderGirafe({
      plot <- get_course_satisfaction_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
    # Generate Discord engagement plot
    output$discord_engagement_plot <- renderGirafe({
      plot <- get_discord_engagement_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
    # Generate learning methods plot
    output$learning_methods_plot <- renderGirafe({
      plot <- get_learning_methods_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
    # Generate community connection plot
    output$community_connection_plot <- renderGirafe({
      plot <- get_community_connection_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
  }
}