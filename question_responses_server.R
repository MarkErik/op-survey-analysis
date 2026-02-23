# =============================================================================
# QUESTION_RESPONSES_SERVER.R - Question Responses Tab Server Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# Server module for Question Responses tab - reactive logic and modal display

questionResponsesServer <- function(id, filteredData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive: Track selected question
    selectedQuestion <- shiny::reactiveVal(NULL)
    
    # Question mappings to data columns
    questionColumns <- list(
      expectations = COL_EXPECTATIONS,
      hopes_gains = COL_EXPECTATIONS,
      improvements = COL_COURSE_IMPROVEMENTS,
      favorite = COL_FAVORITE_PART,
      least_enjoyable = COL_LEAST_ENJOYABLE,
      social_challenges = COL_SOCIAL_CHALLENGES
    )
    
    # Question display titles
    questionTitles <- list(
      expectations = "1. What are your expectations for this course?",
      hopes_gains = "2. What do you hope to gain from this course?",
      improvements = "3. What improvements would you suggest for this course?",
      favorite = "4. What was your favorite part of the course?",
      least_enjoyable = "5. What was the least enjoyable part of the course?",
      social_challenges = "6. What challenges did you face in feeling part of the class?"
    )
    
    # Handle question button clicks
    shiny::observeEvent(input$q1_expectations_btn, { selectedQuestion("expectations") })
    shiny::observeEvent(input$q2_hopes_gains_btn, { selectedQuestion("hopes_gains") })
    shiny::observeEvent(input$q3_improvements_btn, { selectedQuestion("improvements") })
    shiny::observeEvent(input$q4_favorite_btn, { selectedQuestion("favorite") })
    shiny::observeEvent(input$q5_least_enjoyable_btn, { selectedQuestion("least_enjoyable") })
    shiny::observeEvent(input$q6_social_challenges_btn, { selectedQuestion("social_challenges") })
    
    # Update question title when selection changes
    shiny::observe({
      q <- selectedQuestion()
      if (!is.null(q) && !is.null(questionTitles[[q]])) {
        htmltools::runJavaScript(paste0(
          "document.getElementById('", ns("selectedQuestionTitle"), "').textContent = '",
          questionTitles[[q]], "';"
        ))
      }
    })
    
    # Reactive: Get responses for selected question
    questionResponses <- shiny::reactive({
      req(selectedQuestion())
      req(filteredData())
      tryCatch({
        df <- filteredData()
        col <- questionColumns[[selectedQuestion()]]
        if (is.null(col)) return(NULL)
        
        data.frame(
          response_id = df$response_id,
          response = df[[col]],
          select = rep('<i class="bi bi-person-circle"></i> View', nrow(df)),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        warning(paste("Error getting responses:", e$message))
        NULL
      })
    })
    
    # Render responses table
    output$responsesTable <- DT::renderDataTable({
      req(questionResponses())
      tryCatch({
        DT::datatable(
          questionResponses(),
          options = list(
            pageLength = 30,
            scrollX = TRUE,
            ordering = TRUE,
            columnDefs = list(
              list(targets = c(0), visible = FALSE),
              list(targets = c(1), width = "60%"),
              list(targets = c(2), width = "15%", orderable = FALSE)
            )
          ),
          rownames = FALSE,
          escape = -2,
          selection = "single"
        )
      }, error = function(e) {
        warning(paste("Error rendering table:", e$message))
        DT::datatable(data.frame(response = "Error loading responses"))
      })
    })
    
    # Observe row selection and open modal
    shiny::observeEvent(input$responsesTable_rows_selected, {
      req(input$responsesTable_rows_selected)
      req(selectedQuestion())
      req(filteredData())
      
      tryCatch({
        rowIdx <- input$responsesTable_rows_selected[1]
        df <- filteredData()
        responses <- questionResponses()
        
        if (rowIdx > nrow(responses)) return()
        
        participantId <- responses$response_id[rowIdx]
        participant <- df[df$response_id == participantId, ]
        
        if (nrow(participant) == 0) return()
        
        # Build basic info
        section <- coalesce_value(participant$section, "Not specified")
        experience <- coalesce_value(participant$programming_experience, "Not specified")
        preference <- coalesce_value(participant$learning_preference, "Not specified")
        
        # Get selected response
        selectedResp <- coalesce_value(responses$response[rowIdx], "No response provided")
        
        # Get all other free-text responses
        otherQuestions <- list(
          expectations = c("Expectations", COL_EXPECTATIONS),
          improvements = c("Course Improvements", COL_COURSE_IMPROVEMENTS),
          favorite = c("Favorite Part", COL_FAVORITE_PART),
          least_enjoyable = c("Least Enjoyable", COL_LEAST_ENJOYABLE),
          social_challenges = c("Social Challenges", COL_SOCIAL_CHALLENGES),
          inclusivity = c("Inclusivity Feedback", COL_INCLUSIVITY),
          student_interaction = c("Student Interaction", COL_STUDENT_INTERACTION),
          prof_interaction = c("Professor Interaction", COL_PROF_INTERACTION),
          other_comments = c("Other Comments", COL_OTHER_COMMENTS)
        )
        
        otherResponsesList <- list()
        for (qName in names(otherQuestions)) {
          if (qName != selectedQuestion()) {
            colIdx <- otherQuestions[[qName]][2]
            resp <- participant[[colIdx]]
            if (!is.na(resp) & resp != "") {
              otherResponsesList <- c(otherResponsesList, list(
                htmltools::tags$div(class = "response-item",
                  htmltools::tags$p(class = "response-label", otherQuestions[[qName]][1]),
                  htmltools::tags$p(class = "response-content", resp)
                )
              ))
            }
          }
        }
        otherResponses <- htmltools::tags$div(class = "other-responses-list", otherResponsesList)
        
        # Create and show modal
        modal <- shiny::modalDialog(
          title = "Participant Profile",
          size = "l",
          easyClose = TRUE,
          htmltools::tags$div(class = "participant-modal",
            htmltools::tags$div(class = "participant-basic-info",
              htmltools::tags$h4("Basic Information"),
              htmltools::tags$div(class = "info-grid",
                htmltools::tags$p(htmltools::tags$strong("Section: "), section),
                htmltools::tags$p(htmltools::tags$strong("Prior Experience: "), experience),
                htmltools::tags$p(htmltools::tags$strong("Learning Preference: "), preference)
              )
            ),
            htmltools::tags$hr(),
            htmltools::tags$div(class = "participant-selected-response",
              htmltools::tags$h4("Selected Response"),
              htmltools::tags$p(class = "response-text", selectedResp)
            ),
            htmltools::tags$hr(),
            htmltools::tags$div(class = "participant-all-responses",
              htmltools::tags$h4("All Other Responses"),
              otherResponses
            )
          )
        )
        
        shiny::showModal(modal)
        
      }, error = function(e) {
        warning(paste("Error opening modal:", e$message))
      })
    })
  })
}