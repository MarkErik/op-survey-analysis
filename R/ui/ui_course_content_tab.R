# Course Content Tab UI
# Course expectations, agreement statements, and learning preferences

#' Create Course Content Tab
#'
#' Creates the Course Content tab with 6 Likert agreement statements visualizations
#' and learning preferences section.
#'
#' @return A shiny UI element for the Course Content tab
#' @export
create_course_content_tab <- function() {
  tabPanel(
    title = "Course Content",
    value = "course_content",
    div(
      class = "tab-content course-content-tab",
      # Page Header
      div(
        class = "page-header",
        h2("Course Content Analysis", class = "page-title"),
        p("Analysis of course expectations, agreement statements, and learning preferences", class = "page-subtitle")
      ),
      
      # Comparison Controls
      div(
        class = "comparison-section",
        create_comparison_controls(id = "course_content_comparison")
      ),
      
      # Course Agreement Statements Section
      div(
        class = "agreement-section",
        h3("Course Agreement Statements", class = "section-title"),
        p("Responses to 6 Likert-scale agreement statements about course content", class = "section-description"),
        
        # Likert Heatmap for all 6 statements
        fluidRow(
          column(12,
            create_chart_container(
              title = "Agreement Statement Distribution",
              plot_id = "course_agreement_heatmap",
              ns = NS("course_content")
            )
          )
        ),
        
        # Statement Rankings
        fluidRow(
          column(12,
            create_chart_container(
              title = "Statement Rankings (Average Score)",
              plot_id = "course_agreement_rankings",
              ns = NS("course_content")
            )
          )
        ),
        
        # Individual Statement Charts
        fluidRow(
          column(6,
            create_chart_container(
              title = uiOutput("course_statement_1_title"),
              plot_id = "course_statement_1",
              ns = NS("course_content")
            )
          ),
          column(6,
            create_chart_container(
              title = uiOutput("course_statement_2_title"),
              plot_id = "course_statement_2",
              ns = NS("course_content")
            )
          )
        ),
        fluidRow(
          column(6,
            create_chart_container(
              title = uiOutput("course_statement_3_title"),
              plot_id = "course_statement_3",
              ns = NS("course_content")
            )
          ),
          column(6,
            create_chart_container(
              title = uiOutput("course_statement_4_title"),
              plot_id = "course_statement_4",
              ns = NS("course_content")
            )
          )
        ),
        fluidRow(
          column(6,
            create_chart_container(
              title = uiOutput("course_statement_5_title"),
              plot_id = "course_statement_5",
              ns = NS("course_content")
            )
          ),
          column(6,
            create_chart_container(
              title = uiOutput("course_statement_6_title"),
              plot_id = "course_statement_6",
              ns = NS("course_content")
            )
          )
        )
      ),
      
      # Learning Preferences Section
      div(
        class = "preferences-section",
        h3("Learning Preferences", class = "section-title"),
        p("Student preferences for learning format and course expectations", class = "section-description"),
        
        fluidRow(
          # Learning Preference Distribution
          column(6,
            create_chart_container(
              title = "Learning Format Preference",
              plot_id = "learning_preference_distribution",
              ns = NS("course_content")
            )
          ),
          # Expectations Met
          column(6,
            create_chart_container(
              title = "Expectations Met",
              plot_id = "expectations_met_gauge",
              ns = NS("course_content")
            )
          )
        )
      ),
      
      # Section Comparisons
      div(
        class = "comparison-section",
        h3("Section Comparisons", class = "section-title"),
        fluidRow(
          column(12,
            create_chart_container(
              title = "Agreement Scores by Section",
              plot_id = "course_section_comparison",
              ns = NS("course_content")
            )
          )
        ),
        fluidRow(
          column(12,
            create_chart_container(
              title = "Agreement Scores by Experience Level",
              plot_id = "course_experience_comparison",
              ns = NS("course_content")
            )
          )
        )
      ),
      
      # Insights Panel
      div(
        class = "insights-section",
        create_insights_panel(id = "course_content_insights", title = "Course Content Insights")
      )
    )
  )
}
