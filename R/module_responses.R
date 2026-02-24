# R/module_responses.R
# Question Responses tab module for the CPSC Experience Survey Explorer
# Provides UI and server functions for browsing and exploring individual free-text responses

# =============================================================================
# UI Module - Question Responses Tab
# =============================================================================

#' Responses UI module function
#'
#' Creates the Question Responses tab UI with question selector buttons and
#' responses table display.
#'
#' @param id Character module ID for namespacing
#' @return UI element for the Question Responses tab
#' @export
responsesUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Question Selector Section
    fluidRow(
      column(12,
        div(
          class = "question-selector-section",
          h3("Question Responses", class = "section-title"),
          # Horizontal row of clickable buttons for each free-text question
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

    # Responses Table Section
    fluidRow(
      column(12,
        div(
          class = "responses-table-section",
          # Table caption showing selected question
          div(
            class = "table-caption",
            h4(id = ns("responses_caption"), "Select a question to view responses")
          ),
          # DT datatable for displaying responses
          DT::dataTableOutput(ns("responses_table"))
        )
      )
    )
  )
}

# =============================================================================
# Server Module - Question Responses Tab
# =============================================================================

#' Responses module server function
#'
#' Provides reactive data expressions and click handlers for the Question Responses tab.
#'
#' @param id Character module ID for namespacing
#' @param data_server Reactive data server module (optional)
#' @param filter_server Reactive filter server module (optional)
#' @return List of reactive expressions and outputs for the Question Responses tab
#' @export
responsesServer <- function(id, data_server = NULL, filter_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # =============================================================================
    # Reactive Data Access
    # =============================================================================

    #' Reactive expression for filtered data
    filtered_data <- reactive({
      tryCatch({
        if (!is.null(data_server)) {
          data_server$getDataReactive()
        } else {
          # Fallback: use reactive data from module
          reactive({
            tibble::tibble()
          })()
        }
      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    #' Reactive expression for selected question
    selected_question <- reactiveVal(NULL)

    #' Reactive expression for filtered responses
    filtered_responses <- reactive({
      tryCatch({
        data <- filtered_data()
        q_key <- selected_question()

        if (nrow(data) == 0 || is.null(q_key)) {
          return(tibble::tibble())
        }

        # Get the column name for the selected question
        q_col <- FREE_TEXT_QUESTIONS[[q_key]]

        # Filter data for the selected question
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

    # =============================================================================
    # Question Button Click Handlers
    # =============================================================================

    #' Handle question button clicks
    observeEvent(names(FREE_TEXT_QUESTIONS), {
      lapply(names(FREE_TEXT_QUESTIONS), function(q_name) {
        btn_id <- ns(paste0("q_", q_name))
        observeEvent(input[[btn_id]], {
          selected_question(q_name)
        })
      })
    })

    # =============================================================================
    # DT DataTable Output
    # =============================================================================

    #' Render responses table
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

    #' Update table caption
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

    # =============================================================================
    # Individual Response Click Handler
    # =============================================================================

    #' Handle row clicks for viewing individual responses
    observeEvent(input$responses_table_rows_selected, {
      tryCatch({
        selected_rows <- input$responses_table_rows_selected

        if (!is.null(selected_rows) && length(selected_rows) > 0) {
          data <- filtered_responses()
          if (nrow(data) > 0) {
            # Get the selected row data
            selected_row <- data[selected_rows[1], ]

            # Open participant modal with selected response
            showModal(
              participantModalUI(
                participant_data = selected_row,
                selected_response_question = selected_question()
              )
            )
          }
        }
      }, error = function(e) {
        # Silently handle errors
      })
    })

    # Return reactive expressions for use by other modules
    return(list(
      selected_question = selected_question,
      filtered_responses = filtered_responses
    ))
  })
}

# =============================================================================
# Participant Profile Modal UI
# =============================================================================

#' Participant modal UI function
#'
#' Creates a modal dialog displaying participant profile information and
#' all their responses for the selected question.
#'
#' @param participant_data Tibble with participant's data
#' @param selected_response_question Character key of the selected question
#' @return UI element for the participant profile modal
#' @export
participantModalUI <- function(participant_data, selected_response_question = NULL) {
  ns <- NS("participant_modal")

  # Extract participant information
  section <- participant_data[[COL_SECTION]]
  experience <- participant_data[[COL_EXPERIENCE]]
  learning_pref <- participant_data[[COL_LEARNING_PREF]]
  selected_response <- participant_data[[selected_response_question]]

  tagList(
    modalDialog(
      title = "Participant Profile",
      size = "l",  # Large size for comfortable reading
      easyClose = TRUE,  # Easy to close (X button or click outside)
      footer = tagList(
        modalButton("Close")
      ),

      # Basic Information Section
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

      # Selected Response Section
      div(
        class = "selected-response-section",
        h4("Selected Response", class = "info-section-title"),
        div(
          class = "response-content",
          selected_response
        )
      ),

      # All Other Responses Section
      div(
        class = "other-responses-section",
        h4("Other Responses", class = "info-section-title"),
        # Get all other free-text questions
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
