responsesUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(
          class = "question-selector-section",
          h3("Question Responses", class = "section-title"),
          div(
            class = "question-buttons-row",
            lapply(names(FREE_TEXT_QUESTIONS), function(q_name) {
              btn_id <- ns(paste0("q_", q_name))
              actionButton(
                btn_id,
                label = q_name,
                class = "question-btn",
                icon = icon("question")
              )
            })
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "responses-table-section",
          div(
            class = "table-caption",
            h4(id = ns("responses_caption"), "Select a question to view responses")
          ),
          DT::dataTableOutput(ns("responses_table"))
        )
      )
    )
  )
}

responsesServer <- function(id, data_server = NULL, filter_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    filtered_data <- reactive({
      tryCatch({
        if (!is.null(data_server)) {
          data_server$getDataReactive()
        } else {
          reactive({
            tibble::tibble()
          })()
        }
      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    selected_question <- reactiveVal(NULL)

    filtered_responses <- reactive({
      tryCatch({
        data <- filtered_data()
        q_key <- selected_question()

        if (nrow(data) == 0 || is.null(q_key)) {
          return(tibble::tibble())
        }

        q_col <- FREE_TEXT_QUESTIONS[[q_key]]

        responses <- data %>%
          dplyr::filter(dplyr::all_of(q_col) != "") %>%
          dplyr::select(
            dplyr::all_of(COL_TIMESTAMP),
            dplyr::all_of(COL_SECTION),
            dplyr::all_of(COL_EXPERIENCE),
            dplyr::all_of(COL_LEARNING_PREF),
            dplyr::all_of(q_col)
          ) %>%
          dplyr::mutate(
            response_id = row_number(),
            question_key = q_key
          )

        return(responses)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    observeEvent(names(FREE_TEXT_QUESTIONS), {
      lapply(names(FREE_TEXT_QUESTIONS), function(q_name) {
        btn_id <- ns(paste0("q_", q_name))
        observeEvent(input[[btn_id]], {
          selected_question(q_name)
        })
      })
    })

    output$responses_table <- DT::renderDataTable({
      data <- filtered_responses()

      if (nrow(data) == 0) {
        DT::datatable(
          tibble::tibble(
            message = "No responses available for this question"
          ),
          options = list(
            pageLength = DEFAULT_TABLE_PAGE_SIZE,
            scrollX = TABLE_SCROLLX,
            scrollY = TABLE_SCROLLY,
            searching = FALSE,
            ordering = TRUE,
            paging = TRUE,
            info = TRUE,
            lengthChange = FALSE
          ),
          rownames = FALSE,
          selection = "none"
        )
      } else {
        DT::datatable(
          data,
          options = list(
            pageLength = DEFAULT_TABLE_PAGE_SIZE,
            scrollX = TABLE_SCROLLX,
            scrollY = TABLE_SCROLLY,
            searching = FALSE,
            ordering = TRUE,
            paging = TRUE,
            info = TRUE,
            lengthChange = FALSE,
            columnDefs = list(
              list(
                targets = 0,
                visible = FALSE
              )
            )
          ),
          rownames = FALSE,
          selection = "none",
          callback = JS(
            "$(document).ready(function() {",
            "  $('.dataTables_wrapper').addClass('dt-responsive');",
            "});"
          )
        )
      }
    })

    output$responses_caption <- renderText({
      q_key <- selected_question()
      if (!is.null(q_key)) {
        q_name <- names(FREE_TEXT_QUESTIONS)[names(FREE_TEXT_QUESTIONS) == q_key]
        if (length(q_name) > 0) {
          paste("Responses for:", q_name)
        } else {
          "Select a question to view responses"
        }
      } else {
        "Select a question to view responses"
      }
    })

    observeEvent(input$responses_table_rows_selected, {
      tryCatch({
        selected_rows <- input$responses_table_rows_selected

        if (!is.null(selected_rows) && length(selected_rows) > 0) {
          data <- filtered_responses()
          if (nrow(data) > 0) {
            selected_row <- data[selected_rows[1], ]

            showModal(
              participantModalUI(
                participant_data = selected_row,
                selected_response_question = selected_question()
              )
            )
          }
        }
      }, error = function(e) {
      })
    })

    return(list(
      selected_question = selected_question,
      filtered_responses = filtered_responses
    ))
  })
}

participantModalUI <- function(participant_data, selected_response_question = NULL) {
  ns <- NS("participant_modal")

  section <- participant_data[[COL_SECTION]]
  experience <- participant_data[[COL_EXPERIENCE]]
  learning_pref <- participant_data[[COL_LEARNING_PREF]]
  selected_response <- participant_data[[selected_response_question]]

  tagList(
    modalDialog(
      title = "Participant Profile",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      ),

      div(
        class = "participant-info-section",
        h4("Basic Information", class = "info-section-title"),
        div(
          class = "info-row",
          strong("Section:"),
          span(section)
        ),
        div(
          class = "info-row",
          strong("Prior Experience:"),
          span(experience)
        ),
        div(
          class = "info-row",
          strong("Learning Preference:"),
          span(learning_pref)
        )
      ),

      div(
        class = "selected-response-section",
        h4("Selected Response", class = "info-section-title"),
        div(
          class = "response-content",
          selected_response
        )
      ),

      div(
        class = "other-responses-section",
        h4("Other Responses", class = "info-section-title"),
        lapply(names(FREE_TEXT_QUESTIONS), function(q_name) {
          if (q_name != selected_response_question) {
            q_col <- FREE_TEXT_QUESTIONS[[q_name]]
            other_response <- participant_data[[q_col]]
            if (!is.na(other_response) && other_response != "") {
              div(
                class = "other-response-item",
                h5(q_name, class = "other-question-title"),
                div(
                  class = "other-response-content",
                  other_response
                )
              )
            }
          }
        })
      )
    )
  )
}
