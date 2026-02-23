# =============================================================================
# QUESTION_RESPONSES_UI.R - Question Responses Tab UI Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# UI module for Question Responses tab - defines question selector and responses table

questionResponsesUI <- function(id) {
  ns <- NS(id)
  
  htmltools::tags$div(class = "question-responses-tab", role = "region", `aria-label` = "Question responses",
    # Question Selector Section
    htmltools::tags$div(class = "question-selector-section", role = "region", `aria-label` = "Select question",
      htmltools::tags$h3("Select a Free-Text Question"),
      htmltools::tags$div(class = "question-buttons-row",
        # Button 1: Expectations
        htmltools::tags$button(
          id = ns("q1_expectations_btn"),
          class = "question-btn",
          `data-question` = "expectations",
          "1. What are your expectations for this course?"
        ),
        # Button 2: Hopes/Gains
        htmltools::tags$button(
          id = ns("q2_hopes_gains_btn"),
          class = "question-btn",
          `data-question` = "hopes_gains",
          "2. What do you hope to gain from this course?"
        ),
        # Button 3: Course Improvements
        htmltools::tags$button(
          id = ns("q3_improvements_btn"),
          class = "question-btn",
          `data-question` = "improvements",
          "3. What improvements would you suggest for this course?"
        ),
        # Button 4: Favorite Part
        htmltools::tags$button(
          id = ns("q4_favorite_btn"),
          class = "question-btn",
          `data-question` = "favorite",
          "4. What was your favorite part of the course?"
        ),
        # Button 5: Least Enjoyable
        htmltools::tags$button(
          id = ns("q5_least_enjoyable_btn"),
          class = "question-btn",
          `data-question` = "least_enjoyable",
          "5. What was the least enjoyable part of the course?"
        ),
        # Button 6: Social Challenges
        htmltools::tags$button(
          id = ns("q6_social_challenges_btn"),
          class = "question-btn",
          `data-question` = "social_challenges",
          "6. What challenges did you face in feeling part of the class?"
        )
      )
    ),
    
    htmltools::tags$hr(),
    
    # Responses Table Section
    htmltools::tags$div(class = "responses-table-section", role = "region", `aria-label` = "Responses table",
      htmltools::tags$h3(id = ns("selectedQuestionTitle"), "Select a question to view responses"),
      htmltools::tags$div(id = ns("responsesTableWrapper"), class = "dt-responsive",
        DT::dataTableOutput(ns("responsesTable"))
      )
    ),
    
    # Hidden elements for modal data
    htmltools::tags$div(id = ns("modalDataContainer"), style = "display: none;",
      htmltools::tags$span(id = ns("modalSection")),
      htmltools::tags$span(id = ns("modalExperience")),
      htmltools::tags$span(id = ns("modalPreference")),
      htmltools::tags$span(id = ns("modalSelectedResponse")),
      htmltools::tags$div(id = ns("modalOtherResponses"))
    )
  )
}