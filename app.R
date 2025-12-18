# Main application file for the Shiny app

# Load required libraries
library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(stringr)
library(ggplot2)

# Source all R files in the R directory
R_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in R_files) {
  source(file, local = FALSE)
}

# Define UI
ui <- fluidPage(
  # Include custom CSS
  includeCSS("www/css/style.css"),
  
  # Header
  div(class = "app-header",
    h1("Survey Explorer", class = "app-title"),
    p("Explore survey responses with interactive analysis", class = "app-subtitle")
  ),
  
  # Main content area
  fluidRow(
    column(3,
      div(class = "sidebar-panel",
        h3("Navigation"),
        
        # Category buttons for free-text questions
        hr(),
        h4("Free-Text Questions"),
        div(class = "question-buttons",
          lapply(names(free_text_questions), function(question) {
            actionButton(
              inputId = paste0("btn_", question),
              label = free_text_questions[question],
              class = "question-btn"
            )
          })
        )
      )
    ),
    column(9,
      tabsetPanel(
        id = "tabset",
        tabPanel("Home",
          fluidRow(
            column(12,
              div(class = "home-content",
                h2("Welcome to the Survey Explorer"),
                p("This application provides an interactive way to explore survey responses.
                  The free-text questions are organized by category below."),
                
                # Statistics section
                div(class = "stats-container",
                  div(class = "stat-card",
                    h4("Total Responses"),
                    div(class = "stat-value", textOutput("total_responses"))
                  ),
                  div(class = "stat-card",
                    h4("Questions"),
                    div(class = "stat-value", textOutput("question_count"))
                  ),
                  div(class = "stat-card",
                    h4("Avg. Response Length"),
                    div(class = "stat-value", textOutput("avg_response_length"))
                  )
                ),
                
                hr(),
                
                # Data visualization section
                h3("Survey Insights"),
                p("Visual overview of the survey data and response patterns."),
                
                div(class = "visualization-container",
                  fluidRow(
                    column(6,
                      div(class = "chart-container",
                        h4("Response Distribution"),
                        plotOutput("response_distribution_plot", height = "300px")
                      )
                    ),
                    column(6,
                      div(class = "chart-container",
                        h4("Response Length by Question"),
                        plotOutput("response_length_plot", height = "300px")
                      )
                    )
                  )
                ),
                
                hr(),
                
                h3("Free-Text Questions"),
                p("Click on any of the buttons in the sidebar to explore responses to that question."),
                
                div(class = "question-grid",
                  lapply(names(free_text_questions), function(question) {
                    div(class = "question-card",
                      h4(free_text_questions[question]),
                      p("Click the button in the sidebar to view all responses to this question.")
                    )
                  })
                )
              )
            )
          )
        ),
        tabPanel("Question Responses",
          fluidRow(
            column(12,
              h3("Question Responses"),
              p("Select a question from the sidebar to view all responses."),
              
              # DT output for responses table
              DTOutput("responses_table")
            )
          )
        ),
        tabPanel("Participant Profile",
          fluidRow(
            column(12,
              h3("Participant Profile"),
              p("Click on any response in the table to view the participant's complete profile."),
              
              # UI for participant profile
              uiOutput("profile_ui")
            )
          )
        )
      )
    )
  )
)

# Define server
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
    participant_data <- get_participant_profile(df(), selected_row(), current_responses())
    
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
  
  # Update statistics on home page
  output$total_responses <- renderText({
    if (!is.null(df())) {
      nrow(df())
    } else {
      "0"
    }
  })
  
  output$question_count <- renderText({
    length(free_text_questions)
  })
  
  output$avg_response_length <- renderText({
    if (!is.null(df())) {
      # Calculate average response length across all free-text questions
      all_responses <- c()
      for (question in names(free_text_questions)) {
        if (question %in% names(df())) {
          responses <- df()[[question]]
          responses <- responses[!is.na(responses) & responses != ""]
          all_responses <- c(all_responses, nchar(responses))
        }
      }
      if (length(all_responses) > 0) {
        round(mean(all_responses))
      } else {
        "0"
      }
    } else {
      "0"
    }
  })
  
  # Generate response distribution plot
  output$response_distribution_plot <- renderPlot({
    if (!is.null(df())) {
      # Count responses for each question
      response_counts <- sapply(names(free_text_questions), function(question) {
        if (question %in% names(df())) {
          responses <- df()[[question]]
          sum(!is.na(responses) & responses != "")
        } else {
          0
        }
      })
      
      # Create data frame for plotting
      plot_data <- data.frame(
        question = names(free_text_questions),
        count = response_counts,
        question_label = free_text_questions
      )
      
      # Order by count
      plot_data <- plot_data[order(plot_data$count, decreasing = TRUE), ]
      
      # Create bar plot
      ggplot(plot_data, aes(x = reorder(question_label, -count), y = count, fill = count)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none"
        ) +
        labs(x = "Question", y = "Number of Responses", title = "") +
        coord_flip()
    } else {
      # Return empty plot if no data
      ggplot() +
        geom_blank() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    }
  })
  
  # Generate response length plot
  output$response_length_plot <- renderPlot({
    if (!is.null(df())) {
      # Calculate average response length for each question
      avg_lengths <- sapply(names(free_text_questions), function(question) {
        if (question %in% names(df())) {
          responses <- df()[[question]]
          responses <- responses[!is.na(responses) & responses != ""]
          if (length(responses) > 0) {
            mean(nchar(responses))
          } else {
            0
          }
        } else {
          0
        }
      })
      
      # Create data frame for plotting
      plot_data <- data.frame(
        question = names(free_text_questions),
        avg_length = avg_lengths,
        question_label = free_text_questions
      )
      
      # Order by average length
      plot_data <- plot_data[order(plot_data$avg_length, decreasing = TRUE), ]
      
      # Create bar plot
      ggplot(plot_data, aes(x = reorder(question_label, -avg_length), y = avg_length, fill = avg_length)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none"
        ) +
        labs(x = "Question", y = "Average Response Length (characters)", title = "") +
        coord_flip()
    } else {
      # Return empty plot if no data
      ggplot() +
        geom_blank() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "Loading data...", size = 5)
    }
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)