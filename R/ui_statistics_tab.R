# Statistics Tab UI - Detailed Statistical Analysis
# Contains the UI elements for detailed statistical analysis of Likert-scale questions
#
# @author Course Instructor
# @version 2.0.0

#' Statistics Tab UI
#'
#' Creates the complete Statistics tab interface with
#' category selector, question buttons, statistical summary, and distribution visualization
#'
#' @return UI element
#' @export
ui_statistics_tab <- function() {
  div(
    class = "main-content",

    # Page header
    div(
      class = "page-header",
      h1("Statistics", class = "page-title"),
      p("Detailed statistical analysis of Likert-scale questions", class = "page-subtitle")
    ),

    # Section filter display
    uiOutput(outputId = "statistics_section_filter_display"),

    # Category Navigation
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-primary text-white",
        h3("Select Question Category", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Choose a category of questions to analyze:", class = "text-muted mb-3"),
        div(
          class = "category-selector",
          navs_pill(
            id = "stats_category",
            selected = "course_satisfaction",
            nav_item(
              a(
                href = "#",
                onclick = "Shiny.setInputValue('stats_category', 'course_satisfaction'); return false;",
                icon("star"), " Course Satisfaction"
              )
            ),
            nav_item(
              a(
                href = "#",
                onclick = "Shiny.setInputValue('stats_category', 'learning_methods'); return false;",
                icon("book"), " Learning Methods"
              )
            ),
            nav_item(
              a(
                href = "#",
                onclick = "Shiny.setInputValue('stats_category', 'community_belonging'); return false;",
                icon("users"), " Community & Belonging"
              )
            )
          )
        )
      )
    ),

    # Question Selector
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-secondary text-white",
        h3("Select a Question", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Click on a question to view its statistics:", class = "text-muted mb-3"),
        uiOutput(outputId = "stats_question_buttons")
      )
    ),

    # Statistical Summary Panel
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-info text-white",
        h3("Statistical Summary", class = "mb-0")
      ),
      div(
        class = "card-body",
        uiOutput(outputId = "stats_summary_panel")
      )
    ),

    # Distribution Visualization
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-success text-white",
        h3("Response Distribution", class = "mb-0")
      ),
      div(
        class = "card-body",
        plotOutput(
          outputId = "stats_distribution_plot",
          height = "400px"
        )
      )
    ),

    # Section Comparison Toggle
    div(
      class = "card-custom",
      div(
        class = "card-header bg-warning text-dark",
        h3("Section Comparison", class = "mb-0")
      ),
      div(
        class = "card-body",
        div(
          class = "form-check form-switch mb-3",
          tags$input(
            class = "form-check-input",
            type = "checkbox",
            id = "stats_section_comparison",
            `data-value` = "false"
          ),
          tags$label(
            class = "form-check-label",
            `for` = "stats_section_comparison",
            "Compare across sections"
          )
        ),
        uiOutput(outputId = "stats_section_comparison_panel")
      )
    )
  )
}

#' Create statistical summary table
#'
#' Generates a table displaying descriptive statistics
#'
#' @param stats List of summary statistics
#' @return UI element
#' @export
ui_stats_summary_table <- function(stats) {
  if (is.null(stats) || stats$n == 0) {
    return(div(class = "alert alert-warning", "No data available for this question"))
  }

  fluidRow(
    column(
      width = 3,
      div(class = "stat-item",
          h5("Descriptive Statistics"),
          table(class = "table table-sm",
                tr(td("N (valid)"), td(stats$n)),
                tr(td("Mean"), td(round(stats$mean, 2))),
                tr(td("Median"), td(stats$median)),
                tr(td("Mode"), td(stats$mode)),
                tr(td("Std. Deviation"), td(round(stats$sd, 2))),
                tr(td("Std. Error"), td(round(stats$se, 3)))
          )
      )
    ),
    column(
      width = 3,
      div(class = "stat-item",
          h5("Range & Quartiles"),
          table(class = "table table-sm",
                tr(td("Minimum"), td(stats$min)),
                tr(td("Maximum"), td(stats$max)),
                tr(td("Q1 (25th)"), td(stats$q1)),
                tr(td("Q3 (75th)"), td(stats$q3)),
                tr(td("Missing"), td(stats$missing))
          )
      )
    ),
    column(
      width = 6,
      div(class = "stat-item",
          h5("Distribution Summary"),
          plotOutput(outputId = "stats_mini_histogram", height = "200px")
      )
    )
  )
}

#' Create distribution histogram UI
#'
#' Generates the UI for response distribution histogram
#'
#' @param question_col Column name for the question
#' @return UI element
#' @export
ui_distribution_plot <- function(question_col) {
  plotOutput(
    outputId = paste0("dist_", make.names(question_col)),
    height = "400px",
    click = NULL
  )
}

#' Create section comparison panel
#'
#' Generates the UI for comparing responses across sections
#'
#' @param question_col Column name for the question
#' @return UI element
#' @export
ui_section_comparison_panel <- function(question_col) {
  tagList(
    div(
      class = "alert alert-info",
      icon("info-circle"), " This analysis compares how different sections responded to the same question."
    ),
    plotOutput(
      outputId = paste0("section_comp_", make.names(question_col)),
      height = "400px"
    ),
    h4("Section Statistics"),
    DT::dataTableOutput(
      outputId = paste0("section_stats_", make.names(question_col))
    )
  )
}

#' Get category display name
#'
#' Returns a human-readable name for a category
#'
#' @param category Category key
#' @return Character string
#' @export
get_category_display_name <- function(category) {
  switch(category,
    "course_satisfaction" = "Course Satisfaction",
    "learning_methods" = "Learning Methods",
    "community_belonging" = "Community & Belonging",
    category
  )
}

#' Get questions for a category
#'
#' Returns the question columns for a given category
#'
#' @param category Category key
#' @return Vector of column names
#' @export
get_category_questions <- function(category) {
  switch(category,
    "course_satisfaction" = get_course_satisfaction_cols(),
    "learning_methods" = get_learning_methods_cols(),
    "community_belonging" = get_community_belonging_cols(),
    character(0)
  )
}

#' Get shortened labels for a category
#'
#' Returns shortened labels for questions in a category
#'
#' @param category Category key
#' @return Named vector of shortened labels
#' @export
get_category_labels <- function(category) {
  switch(category,
    "course_satisfaction" = get_course_satisfaction_labels(),
    "learning_methods" = get_learning_methods_labels(),
    "community_belonging" = get_community_belonging_labels(),
    character(0)
  )
}

#' Likert scale labels for display
#'
#' Full labels for Likert scale values
#'
#' @return Named vector
#' @export
get_likert_labels <- function() {
  c(
    "1" = "Strongly Disagree",
    "2" = "Disagree",
    "3" = "Neutral",
    "4" = "Agree",
    "5" = "Strongly Agree"
  )
}

#' Likert scale colors
#'
#' Color palette for Likert scale values
#'
#' @return Named vector of hex colors
#' @export
get_likert_colors <- function() {
  c(
    "1" = "#d73027",
    "2" = "#fc8d59",
    "3" = "#fee08b",
    "4" = "#d9ef8b",
    "5" = "#1a9850"
  )
}
