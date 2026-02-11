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
    selected_response_id <- reactiveVal(NULL)
    selected_section <- reactiveVal(NULL)
    selected_plot_item <- reactiveVal(NULL)
    
    # Handle question button clicks
    observe({
      lapply(names(free_text_questions), function(question) {
        observeEvent(input[[paste0("btn_", question)]], {
          current_question(question)
          current_responses(get_responses_for_question(df(), question))
          selected_response_id(NULL)
          
          # Switch to Question Responses tab
          updateTabsetPanel(session, "tabset", selected = "Question Responses")
        })
      })
    })
    
    # Handle section selection from section breakdown plot
    observeEvent(input$section_breakdown_plot_selected, {
      if (!is.null(input$section_breakdown_plot_selected)) {
        selected_section(input$section_breakdown_plot_selected)
        selected_plot_item(input$section_breakdown_plot_selected)
      } else {
        selected_section(NULL)
        selected_plot_item(NULL)
      }
    })
    
    # Handle reset section filter button
    observeEvent(input$reset_section_filter, {
      selected_section(NULL)
      selected_plot_item(NULL)
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
    handle_row_selection(input, selected_response_id, current_responses, session)
    
    # Show modal when a response is selected
    observeEvent(selected_response_id(), {
      if (!is.null(selected_response_id())) {
        showModal(modalDialog(
          title = "Participant Profile",
          render_participant_profile(selected_response_id, current_responses, current_question, df(), free_text_questions),
          footer = NULL,
          size = "l",
          easyClose = TRUE
        ))
      }
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
    
    # Helper function to generate dynamic chart titles
    get_chart_title <- function(base_title, selected_section) {
      if (!is.null(selected_section)) {
        paste0(base_title, " (", selected_section, ")")
      } else {
        paste0(base_title, " (All Sections)")
      }
    }
    
    # Render dynamic chart titles
    output$learning_preference_title <- renderUI({
      h4(get_chart_title("Learning Preference Distribution", selected_section()))
    })
    
    output$prior_experience_title <- renderUI({
      h4(get_chart_title("Prior Programming Experience", selected_section()))
    })
    
    output$course_satisfaction_title <- renderUI({
      h4(get_chart_title("Course Satisfaction Overview", selected_section()))
    })
    
    output$discord_engagement_title <- renderUI({
      h4(get_chart_title("Discord Engagement Metrics", selected_section()))
    })
    
    output$learning_methods_title <- renderUI({
      h4(get_chart_title("Most Valuable Learning Methods", selected_section()))
    })
    
    output$community_connection_title <- renderUI({
      h4(get_chart_title("Community Connection Scores", selected_section()))
    })
    
    # Generate learning preference plot
    output$learning_preference_plot <- renderGirafe({
      plot <- get_learning_preference_plot(filtered_df())
      girafe(ggobj = plot)
    })
    
    # Generate prior experience plot
    output$prior_experience_plot <- renderGirafe({
      plot <- get_prior_experience_plot(filtered_df())
      girafe(
        ggobj = plot,
        width_svg = 10)
    })
    
    # Generate section breakdown pie chart
    output$section_breakdown_plot <- renderGirafe({
      plot <- get_section_breakdown_plot(df())
      girafe(
        ggobj = plot,
        width_svg = 9,
        options = list(
          opts_selection(type = "single", selected = selected_plot_item(), css = "fill:#16a085;"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
    # Generate course satisfaction plot
    output$course_satisfaction_plot <- renderGirafe({
      plot <- get_course_satisfaction_plot(filtered_df())
      girafe(
        ggobj = plot,
        width_svg = 10)
    })
    
    # Generate Discord engagement plot
    output$discord_engagement_plot <- renderGirafe({
      plot <- get_discord_engagement_plot(filtered_df())
      girafe(
        ggobj = plot,
        width_svg = 10)
    })
    
    # Generate learning methods plot
    output$learning_methods_plot <- renderGirafe({
      plot <- get_learning_methods_plot(filtered_df())
      girafe(
        ggobj = plot,
        width_svg = 10)
    })
    
    # Generate community connection plot
    output$community_connection_plot <- renderGirafe({
      plot <- get_community_connection_plot(filtered_df())
      girafe(
        ggobj = plot,
        width_svg = 10)
    })
    
  }
}