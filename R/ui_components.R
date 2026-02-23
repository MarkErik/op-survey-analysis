# UI Components - Navigation and Reusable UI Elements
# Contains navigation sidebar, tab panels, and reusable UI components
#
# @author Course Instructor
# @version 2.0.0

#' Navigation Sidebar
#'
#' Creates the navigation sidebar with section filtering
#'
#' @return UI element
#' @export
ui_navigation_sidebar <- function() {
  tagList(
    div(
      class = "sidebar-section",
      h4("Section Filter", class = "sidebar-title"),
      p("Select a course section to filter all data:", class = "sidebar-hint"),

      selectInput(
        inputId = "section_filter",
        label = NULL,
        choices = c("All Sections" = "", SECTIONS),
        selected = ""
      ),

      actionButton(
        inputId = "reset_filter",
        label = "Reset Filter",
        icon = icon("refresh"),
        class = "btn-sm btn-outline-secondary w-100",
        style = "margin-top: 10px;"
      )
    ),

    div(
      class = "sidebar-section",
      h4("Quick Stats", class = "sidebar-title"),
      uiOutput(outputId = "sidebar_total_responses", inline = TRUE),
      uiOutput(outputId = "sidebar_section_count", inline = TRUE)
    ),

    div(
      class = "sidebar-section",
      h4("Help", class = "sidebar-title"),
      p("Use the tabs above to navigate between different views of the survey data.",
        class = "text-muted small")
    )
  )
}

#' Create a card container
#'
#' Wraps content in a Bootstrap card with optional title
#'
#' @param title Card title
#' @param content UI content to include in card
#' @param icon Optional icon to display with title
#' @return Card UI element
#' @export
ui_card <- function(title = NULL, content, icon = NULL) {
  card <- card(
    class = "card-custom"

  )

  if (!is.null(title)) {
    card <- card(
      card_header(title, class = "card-header-custom"),
      card_body(content),
      class = "card-custom"
    )
  }

  if (!is.null(icon)) {
    title <- tags$span(icon, " ", title)
    card <- card(
      card_header(title, class = "card-header-custom"),
      card_body(content),
      class = "card-custom"
    )
  }

  return(card)
}

#' Section filter display
#'
#' Shows the currently selected section with reset option
#'
#' @param selected_section Currently selected section (reactive)
#' @return UI element
#' @export
ui_section_filter_display <- function(selected_section) {
  renderUI({
    section <- selected_section()

    if (is.null(section) || section == "") {
      return(NULL)
    }

    div(
      class = "section-filter",
      tags$strong("Filtered by: "),
      tags$span(class = "badge bg-primary", section),
      actionButton(
        inputId = "reset_filter_display",
        label = "Reset",
        icon = icon("xmark"),
        class = "btn-sm btn-outline-danger",
        style = "margin-left: 10px;"
      )
    )
  })
}

#' Question button selector
#'
#' Creates a row of clickable buttons for question selection
#'
#' @param questions Vector of question strings
#' @param input_id Base input ID for the buttons
#' @param short_labels Optional vector of shortened labels
#' @return UI element
#' @export
ui_question_buttons <- function(questions, input_id, short_labels = NULL) {
  if (is.null(short_labels)) {
    short_labels <- sapply(questions, function(q) {
      if (nchar(q) > 40) {
        paste0(substr(q, 1, 37), "...")
      } else {
        q
      }
    })
  }

  buttons <- lapply(seq_along(questions), function(i) {
    actionButton(
      inputId = paste0(input_id, "_", i),
      label = short_labels[i],
      class = "btn-outline-primary question-button",
      style = "margin: 3px;"
    )
  })

  div(
    class = "question-button-container",
    buttons
  )
}

#' Statistic display box
#'
#' Creates a box displaying a single statistic
#'
#' @param value The statistic value to display
#' @param label Label describing the statistic
#' @param icon Optional icon
#' @return UI element
#' @export
ui_stat_box <- function(value, label, icon = NULL) {
  div(
    class = "stat-box",
    style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
    if (!is.null(icon)) {
      tags$p(icon, style = "margin-bottom: 5px;")
    },
    tags$p(class = "stat-value", value),
    tags$p(class = "stat-label", label)
  )
}

#' Loading spinner
#'
#' Creates a loading spinner with optional message
#'
#' @param message Optional message to display
#' @return UI element
#' @export
ui_loading <- function(message = "Loading...") {
  div(
    class = "text-center",
    style = "padding: 40px;",
    tags$p(
      class = "spinner-border text-primary",
      role = "status",
      tags$span(class = "visually-hidden", message)
    ),
    tags$p(message, class = "text-muted mt-2")
  )
}

#' Empty state message
#'
#' Displays a message when no data is available
#'
#' @param message Message to display
#' @param icon Optional icon
#' @return UI element
#' @export
ui_empty_state <- function(message, icon = "info-circle") {
  div(
    class = "text-center",
    style = "padding: 40px; color: #6c757d;",
    tags$p(icon(icon, class = "fa-3x")),
    tags$p(message, class = "mt-3")
  )
}

#' Category tab selector
#'
#' Creates a tabset for selecting question categories
#'
#' @param categories Named list of categories
#' @param input_id Input ID for the category selection
#' @return UI element
#' @export
ui_category_tabs <- function(categories, input_id = "category") {
  tabs <- lapply(names(categories), function(cat_name) {
    tabPanel(
      title = categories[[cat_name]]$name,
      value = cat_name
    )
  })

  navs_pill(
    id = input_id,
    .list = tabs
  )
}

#' Participant profile modal
#'
#' Creates a modal dialog showing participant details
#'
#' @param participant_id ID of the participant to display
#' @return UI element
#' @export
ui_participant_modal <- function(participant_id) {
  modalDialog(
    title = "Participant Profile",
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),

    uiOutput(outputId = paste0("participant_profile_", participant_id))
  )
}

#' Distribution histogram
#'
#' Creates a histogram visualization for Likert distribution
#'
#' @param data Data frame with response values
#' @param question_col Column name for the question
#' @return ggplot2 object
#' @export
ui_distribution_histogram <- function(data, question_col) {
  # This will be rendered server-side
  plotOutput(outputId = paste0("hist_", make.names(question_col)))
}

#' Correlation matrix heatmap
#'
#' Creates an interactive correlation matrix
#'
#' @param data Data frame with Likert responses
#' @return ggiraph object
#' @export
ui_correlation_matrix <- function(data) {
  girafeOutput(outputId = "correlation_heatmap", width = "100%", height = "600px")
}

#' Section comparison chart
#'
#' Creates a chart comparing responses across sections
#'
#' @param data Data frame with survey responses
#' @param question_col Column name for the question
#' @return ggplot2 object
#' @export
ui_section_comparison <- function(data, question_col) {
  plotOutput(
    outputId = paste0("section_comp_", make.names(question_col)),
    width = "100%",
    height = "400px"
  )
}

#' Tooltip helper
#'
#' Creates a tooltip for an element
#'
#' @param content The content to display
#' @param tooltip Text to show in tooltip
#' @return UI element
#' @export
ui_tooltip <- function(content, tooltip) {
  tags$span(
    title = tooltip,
    `data-toggle` = "tooltip",
    content
  )
}

#' Info popover
#'
#' Creates an info icon with popover content
#'
#' @param title Popover title
#' @param content Popover content
#' @return UI element
#' @export
ui_info_popover <- function(title, content) {
  tags$span(
    class = "info-popover",
    `data-bs-toggle` = "popover",
    `data-bs-title` = title,
    `data-bs-content` = content,
    icon("info-circle")
  )
}
