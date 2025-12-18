# Server logic for the Shiny app

#' Create server function
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
server <- function(input, output, session) {
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
    if (is.null(current_responses())) {
      return(NULL)
    }
    
    responses <- current_responses()
    
    datatable(
      responses,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "400px",
        searching = TRUE,
        ordering = TRUE,
        info = TRUE,
        autoWidth = TRUE
      ),
      selection = 'single',
      rownames = FALSE,
      caption = paste("Responses to:", free_text_questions[current_question()])
    ) %>%
      formatStyle(
        'response_length',
        target = 'row',
        backgroundColor = styleColorBar(c(0, max(responses$response_length, na.rm = TRUE)), 'lightblue')
      )
  })
  
  # Handle row selection in responses table
  observeEvent(input$responses_table_rows_selected, {
    if (!is.null(input$responses_table_rows_selected) && length(input$responses_table_rows_selected) > 0) {
      selected_row(input$responses_table_rows_selected[1])
      
      # Switch to Participant Profile tab
      updateTabsetPanel(session, "tabset", selected = "Participant Profile")
    }
  })
  
  # Render participant profile
  output$profile_ui <- renderUI({
    if (is.null(selected_row()) || is.null(current_responses())) {
      return(p("No participant selected. Click on a response in the table to view the participant's profile."))
    }
    
    # Get the selected participant's data
    participant_data <- get_participant_profile(df(), selected_row())
    
    if (is.null(participant_data)) {
      return(p("Error loading participant data."))
    }
    
    # Format the profile display
    profile_html <- div(class = "profile-container",
      div(class = "profile-header",
        h4("Participant Information"),
        p(paste("Selected response from:", free_text_questions[current_question()]))
      ),
      
      div(class = "profile-content",
        # Basic information
        fluidRow(
          column(6,
            h5("Basic Information"),
            tags$ul(
              tags$li(strong("Timestamp:"), participant_data$timestamp),
              tags$li(strong("Section:"), participant_data$section),
              tags$li(strong("Prior Experience:"), participant_data$prior_experience),
              tags$li(strong("Learning Preference:"), participant_data$learning_preference)
            )
          ),
          column(6,
            h5("Selected Response"),
            div(class = "response-text",
              p(format_response(participant_data[[current_question()]]))
            )
          )
        ),
        
        hr(),
        
        # All other responses
        h5("All Other Responses"),
        div(class = "all-responses",
          lapply(names(free_text_questions), function(question) {
            if (question != current_question()) {
              div(class = "response-item",
                h6(free_text_questions[question]),
                p(format_response(participant_data[[question]]))
              )
            }
          })
        )
      )
    )
    
    return(profile_html)
  })
  
  # Update home page with question count
  output$home_content <- renderText({
    if (!is.null(df())) {
      total_responses <- nrow(df())
      question_count <- length(free_text_questions)
      paste("Total responses:", total_responses, "| Questions:", question_count)
    } else {
      "Loading data..."
    }
  })
}