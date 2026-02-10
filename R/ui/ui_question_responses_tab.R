# UI Question Responses Tab Component

# Create the question responses tab content
create_question_responses_tab <- function(free_text_questions) {
  tabPanel("Question Responses",
    fluidRow(
      column(12,
        h3("Question Responses"),
        p("Select a question below to view all responses."),
        
        # Free-text question selector
        div(class = "question-selector",
          h4("Select a Question"),
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
        ),
        
        hr(),
        
        # DT output for responses table
        DTOutput("responses_table")
      )
    )
  )
}