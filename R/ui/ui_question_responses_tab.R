# UI Question Responses Tab Component

# Create the question responses tab content
create_question_responses_tab <- function() {
  tabPanel("Question Responses",
    fluidRow(
      column(12,
        h3("Question Responses"),
        p("Select a question from the sidebar to view all responses."),
        
        # DT output for responses table
        DTOutput("responses_table")
      )
    )
  )
}