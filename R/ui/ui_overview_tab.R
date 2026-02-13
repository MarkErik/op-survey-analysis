# Overview Tab UI
# High-level summary of all survey data with context, metrics, and insights

#' Create Overview Tab
#'
#' Creates the Overview tab with context information, key metrics dashboard,
#' and quick insights panel.
#'
#' @return A shiny UI element for the Overview tab
#' @export
create_overview_tab <- function() {
  tabPanel(
    title = "Overview",
    value = "overview",
    div(
      class = "tab-content overview-tab",
      # Page Header
      div(
        class = "page-header",
        h2("Survey Overview", class = "page-title"),
        p("High-level summary of all survey responses and key metrics", class = "page-subtitle")
      ),
      
      # Key Metrics Dashboard
      div(
        class = "metrics-dashboard",
        h3("Key Metrics", class = "section-title"),
        fluidRow(
          column(3,
            create_stat_card(
              title = "Total Responses",
              value = uiOutput("overview_total_responses"),
              subtitle = "Completed surveys"
            )
          ),
          column(3,
            create_stat_card(
              title = "Unique Sections",
              value = uiOutput("overview_unique_sections"),
              subtitle = "Course sections represented"
            )
          ),
          column(3,
            create_stat_card(
              title = "Response Rate",
              value = uiOutput("overview_response_rate"),
              subtitle = "Estimated completion rate"
            )
          ),
          column(3,
            create_stat_card(
              title = "Avg Satisfaction",
              value = uiOutput("overview_avg_satisfaction"),
              subtitle = "Overall course satisfaction"
            )
          )
        )
      ),
      
      # Context & Demographics Section
      div(
        class = "context-section",
        h3("Context & Demographics", class = "section-title"),
        fluidRow(
          # Section Distribution
          column(6,
            create_chart_container(
              title = "Section Distribution",
              plot_id = "overview_section_distribution",
              ns = NS("overview")
            )
          ),
          # Programming Experience
          column(6,
            create_chart_container(
              title = "Programming Experience",
              plot_id = "overview_experience_distribution",
              ns = NS("overview")
            )
          )
        ),
        fluidRow(
          # Learning Preference
          column(6,
            create_chart_container(
              title = "Learning Preference",
              plot_id = "overview_learning_preference",
              ns = NS("overview")
            )
          ),
          # Course Agreement Overview
          column(6,
            create_chart_container(
              title = "Course Agreement Overview",
              plot_id = "overview_course_agreement",
              ns = NS("overview")
            )
          )
        )
      ),
      
      # Quick Insights Panel
      div(
        class = "insights-section",
        h3("Quick Insights", class = "section-title"),
        create_insights_panel(id = "overview_insights", title = "Quick Insights")
      ),
      
      # Comparison Controls
      div(
        class = "comparison-section",
        h3("Compare Across Groups", class = "section-title"),
        create_comparison_controls(id = "overview_comparison")
      )
    )
  )
}
