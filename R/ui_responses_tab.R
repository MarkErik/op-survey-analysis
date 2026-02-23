# Question Responses Tab UI - Free-text Response Browser
# Contains the UI elements for browsing and exploring individual free-text responses
#
# @author Course Instructor
# @version 2.0.0

#' Question Responses Tab UI
#'
#' Creates the complete Question Responses tab interface with
#' question selector, responses table, and participant profile modal
#'
#' @return UI element
#' @export
ui_responses_tab <- function() {
  div(
    class = "main-content",

    # Page header
    div(
      class = "page-header",
      h1("Question Responses", class = "page-title"),
      p("Browse and explore individual free-text responses", class = "page-subtitle")
    ),

    # Section filter display
    uiOutput(outputId = "responses_section_filter_display"),

    # Question Selector Section
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-primary text-white",
        h3("Select a Question", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Click on a question below to view all responses:", class = "text-muted mb-3"),
        div(
          class = "question-selector",
          uiOutput(outputId = "free_text_question_buttons")
        )
      )
    ),

    # Responses Table Section
    div(
      class = "card-custom",
      div(
        class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
        h3(id = "responses_table_title", "Responses", class = "mb-0"),
        span(id = "responses_count", class = "badge bg-light text-dark", "0 responses")
      ),
      div(
        class = "card-body",
        # Search input
        div(
          class = "mb-3",
          textInput(
            inputId = "responses_search",
            label = "Search responses:",
            placeholder = "Type to filter responses...",
            width = "100%"
          )
        ),

        # Responses table
        DT::dataTableOutput(outputId = "responses_table")
      )
    ),

    # Participant Profile Modal (hidden by default, shown on row click)
    uiOutput(outputId = "participant_modal_container")
  )
}

#' Get free-text question definitions with shortened labels
#'
#' Creates a named list of free-text questions with shortened button labels
#'
#' @return Named list with question and label
#' @export
get_free_text_questions <- function() {
  list(
    expectations = list(
      question = FREE_TEXT_QUESTIONS$expectations,
      label = "Course Expectations"
    ),
    preferred_method_reason = list(
      question = FREE_TEXT_QUESTIONS$preferred_method_reason,
      label = "Why Preferred Method"
    ),
    not_preferred_reason = list(
      question = FREE_TEXT_QUESTIONS$not_preferred_reason,
      label = "Why Not Preferred"
    ),
    course_improvements = list(
      question = FREE_TEXT_QUESTIONS$course_improvements,
      label = "Course Improvements"
    ),
    favorite_part = list(
      question = FREE_TEXT_QUESTIONS$favorite_part,
      label = "Favorite Part"
    ),
    least_enjoyable = list(
      question = FREE_TEXT_QUESTIONS$least_enjoyable,
      label = "Least Enjoyable"
    ),
    meeting_challenge = list(
      question = FREE_TEXT_QUESTIONS$meeting_challenge,
      label = "Meeting Challenge"
    ),
    inclusivity = list(
      question = FREE_TEXT_QUESTIONS$inclusivity,
      label = "Inclusivity Feedback"
    ),
    student_interaction = list(
      question = FREE_TEXT_QUESTIONS$student_interaction,
      label = "Student Interaction"
    ),
    professor_interaction = list(
      question = FREE_TEXT_QUESTIONS$professor_interaction,
      label = "Professor Interaction"
    ),
    other_comments = list(
      question = FREE_TEXT_QUESTIONS$other_comments,
      label = "Other Comments"
    )
  )
}

#' Create participant profile modal content
#'
#' Generates the UI for displaying participant details in a modal
#'
#' @param participant_data Data frame row for the participant
#' @return UI element
#' @export
ui_participant_profile <- function(participant_data) {
  if (is.null(participant_data) || nrow(participant_data) == 0) {
    return(div("No participant data available"))
  }

  participant <- participant_data[1, ]

  tagList(
    # Basic Information Section
    h4("Basic Information"),
    fluidRow(
      column(
        width = 4,
        strong("Section:"),
        p(participant[[get_section_col()]])
      ),
      column(
        width = 4,
        strong("Prior Experience:"),
        p(participant[[get_programming_experience_col()]])
      ),
      column(
        width = 4,
        strong("Learning Preference:"),
        p(participant[[get_learning_preference_col()]])
      )
    ),

    hr(),

    # Selected Response Section
    h4("Selected Response"),
    div(
      class = "response-box",
      style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
      textOutput(outputId = "modal_selected_response")
    ),

    hr(),

    # All Other Responses Section
    h4("All Other Responses"),
    uiOutput(outputId = "modal_other_responses")
  )
}

#' Get table column definitions for responses
#'
#' Defines columns for the DT::datatable in responses tab
#'
#' @return List of column definitions
#' @export
get_responses_table_columns <- function() {
  list(
    list(
      title = "ID",
      data = "response_id",
      visible = FALSE
    ),
    list(
      title = "Response",
      data = "response_text",
      width = "80%"
    ),
    list(
      title = "",
      data = NULL,
      width = "20%",
      sortable = FALSE,
      render = JS("function(data, type, row) {
        return '<button class=\"btn btn-sm btn-outline-primary view-profile\" data-id=\"' + row.response_id + '\">View Profile</button>';
      }")
    )
  )
}

#' Get DT options for responses table
#'
#' Returns DataTables configuration options
#'
#' @return List of DT options
#' @export
get_responses_table_options <- function() {
  list(
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
    )
  )
}
