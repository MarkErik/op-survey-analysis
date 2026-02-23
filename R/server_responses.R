# Question Responses Tab Server Logic
# Contains reactive logic for browsing and exploring free-text responses
#
# @author Course Instructor
# @version 2.0.0

#' Question Responses Tab Server Function
#'
#' Contains all reactive logic for the Question Responses tab
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv Reactive values containing shared state
#' @export
server_responses <- function(input, output, session, rv) {

  # Reactive value for selected question
  selected_question <- reactiveVal(NULL)

  # Section filter display
  output$responses_section_filter_display <- renderUI({
    section <- rv$selected_section

    if (is.null(section) || section == "") {
      return(NULL)
    }

    div(
      class = "section-filter mb-3",
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px;",
      tags$strong("Filtered by: "),
      tags$span(class = "badge bg-primary", section),
      actionButton(
        inputId = "reset_responses_filter",
        label = "Reset",
        icon = icon("xmark"),
        class = "btn-sm btn-outline-danger",
        style = "margin-left: 10px;"
      )
    )
  })

  # Reset filter handler
  observeEvent(input$reset_responses_filter, {
    rv$selected_section <- NULL
    rv$current_data <- SURVEY_DATA
    updateSelectInput(session, "section_filter", selected = "")
  })

  # Free text question buttons
  output$free_text_question_buttons <- renderUI({
    questions <- get_free_text_questions()

    buttons <- lapply(names(questions), function(q_id) {
      q_info <- questions[[q_id]]

      # Check if question column exists in data
      if (!q_info$question %in% colnames(SURVEY_DATA)) {
        return(NULL)
      }

      actionButton(
        inputId = paste0("select_question_", q_id),
        label = q_info$label,
        class = "btn-outline-primary question-button",
        style = "margin: 3px;"
      )
    })

    div(class = "question-button-container", buttons)
  })

  # Question button observers
  observe({
    questions <- get_free_text_questions()

    lapply(names(questions), function(q_id) {
      observeEvent(input[[paste0("select_question_", q_id)]], {
        q_info <- questions[[q_id]]
        selected_question(q_info$question)

        # Update table title
        session$sendCustomMessage(
          type = "updateResponsesTitle",
          message = paste("Responses:", q_info$label)
        )
      })
    })
  })

  # Filtered responses data
  filtered_responses <- reactive({
    data <- rv$current_data
    question <- selected_question()

    if (is.null(question) || question == "") {
      return(data.frame())
    }

    if (!question %in% colnames(data)) {
      return(data.frame())
    }

    # Get responses
    responses <- data[, c("response_id", question)]
    colnames(responses) <- c("response_id", "response_text")

    # Filter out empty responses
    responses <- responses %>%
      dplyr::filter(!is.na(response_text) & response_text != "")

    # Apply search filter
    search_term <- input$responses_search
    if (!is.null(search_term) && search_term != "") {
      responses <- responses %>%
        dplyr::filter(stringr::str_detect(
          tolower(response_text),
          tolower(search_term)
        ))
    }

    responses
  })

  # Responses count
  output$responses_count <- renderText({
    n <- nrow(filtered_responses())
    paste0(n, " response", ifelse(n != 1, "s", ""))
  })

  # Responses table
  output$responses_table <- DT::renderDataTable({
    responses <- filtered_responses()

    if (nrow(responses) == 0) {
      return(NULL)
    }

    DT::datatable(
      responses,
      selection = "single",
      options = list(
        pageLength = 30,
        lengthMenu = c(10, 30, 50, 100),
        searching = TRUE,
        ordering = TRUE,
        scrollX = TRUE,
        language = list(
          info = "Showing _START_ to _END_ of _TOTAL_ responses",
          infoEmpty = "No responses to show",
          emptyTable = "Select a question to view responses",
          zeroRecords = "No matching responses found"
        ),
        columnDefs = list(
          list(visible = FALSE, targets = 0)
        )
      ),
      rownames = FALSE,
      callback = JS("
        table.on('click', 'tr', function() {
          var data = table.row(this).data();
          if (data) {
            Shiny.setInputValue('selected_response_id', data.response_id, {priority: 'event'});
          }
        });
        return false;
      ")
    )
  })

  # Selected response ID
  observeEvent(input$selected_response_id, {
    req(input$selected_response_id)

    # Show participant modal
    showModal(ui_participant_modal(input$selected_response_id))
  })

  # Participant modal container
  output$participant_modal_container <- renderUI({
    # This is handled by showModal
    NULL
  })

  # Participant profile modal
  observeEvent(input$selected_response_id, {
    req(input$selected_response_id)

    response_id <- input$selected_response_id
    data <- rv$current_data

    participant <- data %>% dplyr::filter(response_id == !!response_id)

    if (nrow(participant) == 0) {
      return(NULL)
    }

    # Build modal content
    modal_content <- ui_participant_profile(participant)

    showModal(modalDialog(
      title = "Participant Profile",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      modal_content
    ))
  })

  # Modal selected response output
  output$modal_selected_response <- renderText({
    req(input$selected_response_id)

    response_id <- input$selected_response_id
    question <- selected_question()

    if (is.null(question) || question == "") {
      return("")
    }

    data <- rv$current_data
    participant <- data %>% dplyr::filter(response_id == !!response_id)

    if (nrow(participant) == 0) {
      return("")
    }

    participant[[question]]
  })

  # Modal other responses
  output$modal_other_responses <- renderUI({
    req(input$selected_response_id)

    response_id <- input$selected_response_id
    current_question <- selected_question()
    data <- rv$current_data

    participant <- data %>% dplyr::filter(response_id == !!response_id)

    if (nrow(participant) == 0) {
      return(NULL)
    }

    # Get all free text questions except current
    questions <- get_free_text_questions()
    other_questions <- names(questions)[sapply(names(questions), function(q_id) {
      questions[[q_id]]$question != current_question
    })]

    # Build response list
    response_list <- lapply(other_questions, function(q_id) {
      q_info <- questions[[q_id]]
      q_col <- q_info$question

      if (q_col %in% colnames(participant)) {
        response <- participant[[q_col]]

        if (!is.na(response) && response != "") {
          div(
            class = "mb-3",
            h5(q_info$label),
            p(response, class = "text-muted")
          )
        }
      }
    })

    # Filter out NULLs
    response_list <- response_list[!sapply(response_list, is.null)]

    if (length(response_list) == 0) {
      return(div(class = "alert alert-info", "No other responses from this participant"))
    }

    tagList(response_list)
  })
}

#' Get participant details for modal
#'
#' Extracts participant information for display
#'
#' @param data Survey data
#' @param response_id Response ID to look up
#' @return List of participant details
#' @export
get_participant_details <- function(data, response_id) {
  participant <- data %>% dplyr::filter(response_id == !!response_id)

  if (nrow(participant) == 0) {
    return(NULL)
  }

  list(
    section = participant[[get_section_col()]],
    experience = participant[[get_programming_experience_col()]],
    preference = participant[[get_learning_preference_col()]]
  )
}

#' Get all responses for a participant
#'
#' Extracts all free-text responses for a participant
#'
#' @param data Survey data
#' @param response_id Response ID to look up
#' @return Data frame of responses
#' @export
get_participant_responses <- function(data, response_id) {
  participant <- data %>% dplyr::filter(response_id == !!response_id)

  if (nrow(participant) == 0) {
    return(data.frame())
  }

  questions <- get_free_text_questions()

  responses <- lapply(names(questions), function(q_id) {
    q_info <- questions[[q_id]]
    q_col <- q_info$question

    if (q_col %in% colnames(participant)) {
      response <- participant[[q_col]]
      if (!is.na(response) && response != "") {
        data.frame(
          question_id = q_id,
          question_label = q_info$label,
          response = response,
          stringsAsFactors = FALSE
        )
      }
    }
  })

  responses <- responses[!sapply(responses, is.null)]
  do.call(rbind, responses)
}
