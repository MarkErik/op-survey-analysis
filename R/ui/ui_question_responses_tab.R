# UI Question Responses Tab Component

# Create the question responses tab content
create_question_responses_tab <- function(free_text_questions) {
  tabPanel("Question Responses",
    div(class = "responses-container",
      # Question selector section
      div(class = "question-selector-section",
        h3("Select a Question"),
        p("Choose a free-text question to view all responses below.", class = "selector-description"),
        div(class = "question-buttons-horizontal",
          lapply(names(free_text_questions), function(question) {
            actionButton(
              inputId = paste0("btn_", question),
              label = free_text_questions[question],
              class = "question-btn-horizontal",
              aria.label = paste("View responses for:", free_text_questions[question])
            )
          })
        )
      ),
      
      # Responses header
      div(class = "responses-header",
        h3("Responses")
      ),
      DTOutput("responses_table")
    )
  )
}