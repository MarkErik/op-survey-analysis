# UI Sidebar Component

# Create the sidebar navigation panel
create_sidebar_panel <- function(free_text_questions) {
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
}