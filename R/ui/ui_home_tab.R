# UI Home Tab Component

# Create the home tab content
create_home_tab <- function() {
  tabPanel("Home",
    fluidRow(
      column(12,
        div(class = "home-content",
          # Overview section
          div(class = "stats-section",
            h3("Overview"),
            p("Select a course section to filter data below"),
            
            # First row: Section chart and Total Responses
            div(class = "overview-first-row",
              fluidRow(
                column(8,
                  div(class = "chart-container",
                    h4("Responses per Section"),
                    girafeOutput("section_breakdown_plot", height = "300px")
                  )
                ),
                column(4,
                  div(class = "stat-card",
                    h4("Total Responses"),
                    div(class = "stat-value", textOutput("total_responses"))
                  )
                )
              )
            ),
            
            # Second row: Selected Section display and Reset button
            div(class = "overview-second-row",
              div(class = "stat-card",
                h4("Selected Section"),
                div(class = "stat-value", textOutput("selected_section_display")),
                actionButton("reset_section_filter", "Reset Filter", class = "btn-reset")
              )
            )
          ),
          
          # Demographics & Preferences
          div(class = "insights-section",
            h3("Demographics & Preferences"),
            p("Overview of student backgrounds and learning preferences."),
            
            div(class = "visualization-container",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4("Learning Preference Distribution"),
                    girafeOutput("learning_preference_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4("Prior Programming Experience"),
                    girafeOutput("prior_experience_plot", height = "300px")
                  )
                )
              )
            )
          ),
          
          # Course Satisfaction & Discord
          div(class = "insights-section",
            h3("Course Satisfaction & Engagement"),
            p("Student satisfaction with the course and Discord platform engagement."),
            
            div(class = "visualization-container",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4("Course Satisfaction Overview"),
                    girafeOutput("course_satisfaction_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4("Discord Engagement Metrics"),
                    girafeOutput("discord_engagement_plot", height = "300px")
                  )
                )
              )
            )
          ),
          
          # Learning Methods & Community Connection
          div(class = "insights-section",
            h3("Learning Methods & Community Connection"),
            p("Most valuable learning methods and community connection scores."),
            
            div(class = "visualization-container",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4("Most Valuable Learning Methods"),
                    girafeOutput("learning_methods_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4("Community Connection Scores"),
                    girafeOutput("community_connection_plot", height = "300px")
                  )
                )
              )
            )
          ),
          
        )
      )
    )
  )
}
