# UI components for the Shiny app

#' Create the main UI layout
#' @param title Application title
#' @return Main UI layout
create_main_ui <- function(title) {
  tagList(
    # Include custom CSS
    includeCSS("www/css/style.css"),
    
    # Header
    header_ui(title),
    
    # Main content area
    fluidRow(
      column(3, sidebar_ui()),
      column(9, main_content_ui())
    )
  )
}

#' Create header UI
#' @param title Application title
#' @return Header UI
header_ui <- function(title) {
  fluidRow(
    column(12,
      div(class = "app-header",
        h1(title, class = "app-title"),
        p("Explore survey responses with interactive analysis", class = "app-subtitle")
      )
    )
  )
}

#' Create sidebar UI
#' @return Sidebar UI
sidebar_ui <- function() {
  sidebarPanel(
    width = 3,
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
    ),
    
    hr(),
    h4("About"),
    p("This application allows you to explore free-text responses from the survey. 
      Click on any question button to view all responses, then click on a response 
      to see the participant's complete profile.")
  )
}

#' Create main content UI
#' @return Main content UI
main_content_ui <- function() {
  tabsetPanel(
    tabPanel("Home", home_ui()),
    tabPanel("Question Responses", question_responses_ui()),
    tabPanel("Participant Profile", participant_profile_ui())
  )
}

#' Create home UI
#' @return Home UI
home_ui <- function() {
  fluidRow(
    column(12,
      div(class = "home-content",
        h2("Welcome to the Survey Explorer"),
        p("This application provides an interactive way to explore survey responses.
          Use the navigation in the sidebar to explore different questions and responses."),
        
        hr(),
        
      )
    )
  )
}

#' Create question responses UI
#' @return Question responses UI
question_responses_ui <- function() {
  fluidRow(
    column(12,
      h3("Question Responses"),
      p("Select a question from the sidebar to view all responses."),
      
      # DT output for responses table
      DTOutput("responses_table")
    )
  )
}

#' Create participant profile UI
#' @return Participant profile UI
participant_profile_ui <- function() {
  fluidRow(
    column(12,
      h3("Participant Profile"),
      p("Click on any response in the table to view the participant's complete profile."),
      
      # UI for participant profile
      uiOutput("profile_ui")
    )
  )
}