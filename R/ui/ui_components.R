# Reusable UI Components
# Shared components used across multiple tabs

#' Create Comparison Controls
#'
#' Creates a panel with selectors for filtering data by section, experience level,
#' and learning preference. Used across all tabs for consistent comparison functionality.
#'
#' @param id A unique identifier for the controls (for namespacing)
#' @return A shiny UI element containing the comparison controls
#' @export
create_comparison_controls <- function(id = NULL) {
  ns <- if (!is.null(id)) shiny::NS(id) else function(x) x
  
  div(
    class = "comparison-controls",
    div(
      class = "controls-header",
      h4("Comparison Filters", class = "controls-title"),
      actionButton(
        ns("reset_filters"),
        "Reset Filters",
        class = "btn-reset",
        icon = icon("refresh")
      )
    ),
    div(
      class = "controls-body",
      # Section Selector
      div(
        class = "control-group",
        tags$label("Section", `for` = ns("section_filter"), class = "control-label"),
        selectInput(
          ns("section_filter"),
          label = NULL,
          choices = c("All Sections" = "all"),
          selected = "all",
          multiple = TRUE,
          selectize = FALSE
        )
      ),
      # Experience Level Selector
      div(
        class = "control-group",
        tags$label("Programming Experience", `for` = ns("experience_filter"), class = "control-label"),
        selectInput(
          ns("experience_filter"),
          label = NULL,
          choices = c(
            "All Levels" = "all",
            "No Experience" = "none",
            "Some Experience" = "some",
            "Highly Experienced" = "high"
          ),
          selected = "all",
          multiple = TRUE,
          selectize = FALSE
        )
      ),
      # Learning Preference Selector
      div(
        class = "control-group",
        tags$label("Learning Preference", `for` = ns("preference_filter"), class = "control-label"),
        selectInput(
          ns("preference_filter"),
          label = NULL,
          choices = c(
            "All Preferences" = "all",
            "In-person" = "in_person",
            "Online" = "online",
            "No Preference" = "no_preference"
          ),
          selected = "all",
          multiple = TRUE,
          selectize = FALSE
        )
      )
    )
  )
}

#' Create Insights Panel
#'
#' Creates a panel for displaying insights, statistical highlights, and recommendations.
#' Used across all tabs to show data-driven insights.
#'
#' @param id A unique identifier for the panel (for namespacing)
#' @param title Optional title for the panel (default: "Insights")
#' @return A shiny UI element containing the insights panel
#' @export
create_insights_panel <- function(id = NULL, title = "Insights") {
  ns <- if (!is.null(id)) shiny::NS(id) else function(x) x
  
  div(
    class = "insights-panel",
    div(
      class = "insights-header",
      h4(title, class = "insights-title"),
      icon("lightbulb", class = "insights-icon")
    ),
    div(
      class = "insights-body",
      # Key Findings Section
      div(
        class = "insights-section",
        h5("Key Findings", class = "insights-section-title"),
        uiOutput(ns("key_findings"))
      ),
      # Statistical Highlights Section
      div(
        class = "insights-section",
        h5("Statistical Highlights", class = "insights-section-title"),
        uiOutput(ns("statistical_highlights"))
      ),
      # Notable Patterns Section
      div(
        class = "insights-section",
        h5("Notable Patterns", class = "insights-section-title"),
        uiOutput(ns("notable_patterns"))
      ),
      # Recommendations Section
      div(
        class = "insights-section",
        h5("Recommendations", class = "insights-section-title"),
        uiOutput(ns("recommendations"))
      )
    )
  )
}

#' Create Participant Profile Modal
#'
#' Creates a modal dialog for displaying a participant's complete survey responses.
#' Includes navigation to previous/next participants and all response sections.
#'
#' @param id A unique identifier for the modal (for namespacing)
#' @return A shiny UI element containing the participant modal definition
#' @export
create_participant_modal <- function(id = NULL) {
  ns <- if (!is.null(id)) shiny::NS(id) else function(x) x
  
  modalDialog(
    title = NULL,
    size = "l",
    footer = NULL,
    easyClose = TRUE,
    div(
      class = "participant-modal",
      # Modal Header
      div(
        class = "modal-header-custom",
        div(
          class = "participant-info",
          h4(uiOutput(ns("participant_title")), class = "participant-title"),
          div(
            class = "participant-meta",
            span(uiOutput(ns("participant_section")), class = "meta-item"),
            span(uiOutput(ns("participant_experience")), class = "meta-item")
          )
        ),
        actionButton(
          ns("close_modal"),
          icon("times"),
          class = "btn-close-modal"
        )
      ),
      # Navigation
      div(
        class = "participant-navigation",
        actionButton(
          ns("prev_participant"),
          icon("chevron-left"),
          "Previous",
          class = "btn-nav btn-prev"
        ),
        span(uiOutput(ns("participant_counter")), class = "participant-counter"),
        actionButton(
          ns("next_participant"),
          "Next",
          icon("chevron-right"),
          class = "btn-nav btn-next"
        )
      ),
      # Modal Body
      div(
        class = "modal-body-custom",
        # Context Section
        div(
          class = "response-section",
          h5("Context & Preferences", class = "section-title"),
          div(
            class = "response-grid",
            div(
              class = "response-item",
              tags$label("Learning Preference:", class = "response-label"),
              uiOutput(ns("participant_preference"))
            ),
            div(
              class = "response-item",
              tags$label("Section:", class = "response-label"),
              uiOutput(ns("participant_section_detail"))
            )
          )
        ),
        # Course Content Section
        div(
          class = "response-section",
          h5("Course Content Agreement", class = "section-title"),
          uiOutput(ns("participant_course_agreement"))
        ),
        # Learning Elements Section
        div(
          class = "response-section",
          h5("Learning Elements", class = "section-title"),
          uiOutput(ns("participant_learning_elements"))
        ),
        # Community Section
        div(
          class = "response-section",
          h5("Community & Belonging", class = "section-title"),
          uiOutput(ns("participant_community"))
        ),
        # Discord Usage Section
        div(
          class = "response-section",
          h5("Discord Usage", class = "section-title"),
          uiOutput(ns("participant_discord"))
        ),
        # Free Text Section
        div(
          class = "response-section",
          h5("Free Text Responses", class = "section-title"),
          uiOutput(ns("participant_free_text"))
        )
      ),
      # Modal Footer
      div(
        class = "modal-footer-custom",
        actionButton(
          ns("view_in_table"),
          "View in Data Table",
          icon = icon("table"),
          class = "btn-secondary"
        )
      )
    )
  )
}

#' Create Stat Card
#'
#' Creates a stat card for displaying key metrics with title, value, and subtitle.
#'
#' @param title The title of the stat card
#' @param value The main value to display
#' @param subtitle Optional subtitle or description
#' @param icon Optional icon name (from shiny::icon)
#' @return A shiny UI element containing the stat card
#' @export
create_stat_card <- function(title, value, subtitle = NULL, icon = NULL) {
  div(
    class = "stat-card",
    if (!is.null(icon)) {
      div(
        class = "stat-icon",
        icon(icon)
      )
    },
    div(
      class = "stat-content",
      h6(title, class = "stat-title"),
      div(value, class = "stat-value"),
      if (!is.null(subtitle)) {
        p(subtitle, class = "stat-subtitle")
      }
    )
  )
}

#' Create Chart Container
#'
#' Creates a standardized container for charts with title and optional controls.
#'
#' @param title The chart title
#' @param plot_id The output ID for the plot
#' @param ns The namespace function
#' @param controls Optional UI elements for chart controls
#' @return A shiny UI element containing the chart container
#' @export
create_chart_container <- function(title, plot_id, ns, controls = NULL) {
  div(
    class = "chart-container",
    div(
      class = "chart-header",
      h5(title, class = "chart-title"),
      if (!is.null(controls)) {
        div(controls, class = "chart-controls")
      }
    ),
    div(
      class = "chart-body",
      plotOutput(ns(plot_id), height = "400px")
    )
  )
}

#' Create Likert Response Display
#'
#' Creates a visual display of a Likert scale response with color coding.
#'
#' @param value The Likert value (1-5)
#' @param label The display label for the response
#' @return A shiny UI element with color-coded Likert display
#' @export
create_likert_display <- function(value, label) {
  color_class <- switch(
    as.character(value),
    "1" = "likert-strongly-disagree",
    "2" = "likert-disagree",
    "3" = "likert-neutral",
    "4" = "likert-agree",
    "5" = "likert-strongly-agree",
    "likert-neutral"
  )
  
  div(
    class = paste("likert-display", color_class),
    span(value, class = "likert-value"),
    span(label, class = "likert-label")
  )
}
