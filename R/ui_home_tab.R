# Home Tab UI - Response Overview and Visualizations
# Contains the UI elements for the Home tab including response overview
# and six visualization panels
#
# @author Course Instructor
# @version 2.0.0

#' Home Tab UI
#'
#' Creates the complete Home tab interface with response overview
#' and six visualization panels
#'
#' @return UI element
#' @export
ui_home_tab <- function() {
  div(
    class = "main-content",

    # Page header
    div(
      class = "page-header",
      h1("Survey Dashboard", class = "page-title"),
      p("Overview of CPSC Course Experience Survey Responses", class = "page-subtitle")
    ),

    # Section filter display
    uiOutput(outputId = "home_section_filter_display"),

    # Response Overview Section
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-primary text-white",
        h3("Response Overview", class = "mb-0")
      ),
      div(
        class = "card-body",
        fluidRow(
          # Total responses counter
          column(
            width = 4,
            div(
              class = "stat-box",
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 10px; padding: 25px;",
              h2(textOutput(outputId = "total_responses"), class = "mb-0"),
              p("Total Responses", class = "mb-0 mt-2", style = "opacity: 0.9;")
            )
          ),

          # Section breakdown
          column(
            width = 8,
            h4("Responses by Section"),
            plotOutput(
              outputId = "section_breakdown_chart",
              height = "200px",
              click = "section_click"
            )
          )
        )
      )
    ),

    # Six Visualization Panels
    fluidRow(
      # Panel 1: Learning Preference Distribution
      column(
        width = 6,
        div(
          class = "card-custom viz-container",
          h4("Learning Preference Distribution"),
          p(class = "chart-subtitle", "(All Sections)"),
          plotOutput(outputId = "learning_preference_chart", height = "300px")
        )
      ),

      # Panel 2: Prior Programming Experience
      column(
        width = 6,
        div(
          class = "card-custom viz-container",
          h4("Prior Programming Experience"),
          p(class = "chart-subtitle", "(All Sections)"),
          plotOutput(outputId = "programming_experience_chart", height = "300px")
        )
      )
    ),

    fluidRow(
      # Panel 3: Course Satisfaction Overview
      column(
        width = 12,
        div(
          class = "card-custom viz-container",
          h4("Course Satisfaction Overview"),
          p(class = "chart-subtitle", "(All Sections)"),
          plotOutput(outputId = "course_satisfaction_chart", height = "350px")
        )
      )
    ),

    fluidRow(
      # Panel 4: Discord Engagement Metrics
      column(
        width = 6,
        div(
          class = "card-custom viz-container",
          h4("Discord Engagement Metrics"),
          p(class = "chart-subtitle", "(All Sections)"),
          plotOutput(outputId = "discord_engagement_chart", height = "300px")
        )
      ),

      # Panel 5: Most Valuable Learning Methods
      column(
        width = 6,
        div(
          class = "card-custom viz-container",
          h4("Most Valuable Learning Methods"),
          p(class = "chart-subtitle", "(All Sections)"),
          plotOutput(outputId = "learning_methods_chart", height = "300px")
        )
      )
    ),

    fluidRow(
      # Panel 6: Community Connection Scores
      column(
        width = 12,
        div(
          class = "card-custom viz-container",
          h4("Community Connection Scores"),
          p(class = "chart-subtitle", "(All Sections)"),
          plotOutput(outputId = "community_scores_chart", height = "350px")
        )
      )
    ),

    # Hidden elements for section filtering
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('#section_click').hide();
      });
    "))
  )
}

#' Get learning preference column name
#'
#' @return Column name string
#' @export
get_learning_preference_col <- function() {
  "Do you prefer in-person or online learning?"
}

#' Get programming experience column name
#'
#' @return Column name string
#' @export
get_programming_experience_col <- function() {
  "Prior to taking this course, what was your programming experience?"
}

#' Get section column name
#'
#' @return Column name string
#' @export
get_section_col <- function() {
  "What section are you in?"
}

#' Get course satisfaction question columns
#'
#' @return Vector of column names
#' @export
get_course_satisfaction_cols <- function() {
  c(
    "How much do you agree with the statement? [The content is relevant and up-to-date]",
    "How much do you agree with the statement? [I am excited about the content and material that I'm learning]",
    "How much do you agree with the statement? [I'm satisfied with the level of feedback I receive]",
    "How much do you agree with the statement? [I feel like I could take what I'm learning and apply it in a new scenario]",
    "How much do you agree with the statement? [It's easy to ask for help]",
    "How much do you agree with the statement? [I feel like I am meeting the goals of learning Python in this course]"
  )
}

#' Get learning methods question columns
#'
#' @return Vector of column names
#' @export
get_learning_methods_cols <- function() {
  c(
    "How much do the following elements contribute to your learning? [Explanations of pre-written code]",
    "How much do the following elements contribute to your learning? [Studying for midterms]",
    "How much do the following elements contribute to your learning? [TopHat Quizzes]",
    "How much do the following elements contribute to your learning? [Presentation slides]",
    "How much do the following elements contribute to your learning? [Post-class handouts and notes]",
    "How much do the following elements contribute to your learning? [Coding on my own]",
    "How much do the following elements contribute to your learning? [Live coding by the professor]",
    "How much do the following elements contribute to your learning? [Labs]",
    "How much do the following elements contribute to your learning? [Being able to ask questions of the professor during lecture]",
    "How much do the following elements contribute to your learning? [Assignments]"
  )
}

#' Get community belonging question columns
#'
#' @return Vector of column names
#' @export
get_community_belonging_cols <- function() {
  c(
    "How much do you agree with the following statements? [I feel comfortable speaking up in class]",
    "How much do you agree with the following statements? [I feel like I am a part of this class]",
    "How much do you agree with the following statements? [Making friends within the class is important to me]",
    "How much do you agree with the following statements? [I feel like I am a part of the university community]",
    "How much do you agree with the following statements? [It's easy to meet new people within the class]"
  )
}

#' Shortened labels for course satisfaction questions
#'
#' @return Named vector of shortened labels
#' @export
get_course_satisfaction_labels <- function() {
  c(
    "Content relevant & up-to-date",
    "Excited about material",
    "Satisfied with feedback",
    "Can apply to new scenarios",
    "Easy to ask for help",
    "Meeting Python goals"
  )
}

#' Shortened labels for learning methods questions
#'
#' @return Named vector of shortened labels
#' @export
get_learning_methods_labels <- function() {
  c(
    "Code explanations",
    "Studying for midterms",
    "TopHat Quizzes",
    "Presentation slides",
    "Handouts & notes",
    "Coding on own",
    "Live coding",
    "Labs",
    "Asking questions",
    "Assignments"
  )
}

#' Shortened labels for community belonging questions
#'
#' @return Named vector of shortened labels
#' @export
get_community_belonging_labels <- function() {
  c(
    "Comfortable speaking up",
    "Feel part of class",
    "Making friends important",
    "Part of university",
    "Easy to meet people"
  )
}
