# UI Sidebar Component

# Create the sidebar navigation panel
create_sidebar_panel <- function(free_text_questions) {
  div(class = "sidebar-panel",
    div(class = "sidebar-header",
      h3("Navigation"),
      p("Select a question to explore responses", class = "sidebar-description")
    ),
    
    # Category buttons for free-text questions
    hr(),
    div(class = "question-section",
      h4("Free-Text Questions"),
      div(class = "question-buttons",
        lapply(names(free_text_questions), function(question) {
          actionButton(
            inputId = paste0("btn_", question),
            label = free_text_questions[question],
            class = "question-btn",
            aria.label = paste("View responses for:", free_text_questions[question])
          )
        })
      )
    )
  )
}