# UI Home Tab Component

# Create the home tab content
create_home_tab <- function() {
  tabPanel("Home",
    fluidRow(
      column(12,
        div(class = "home-content",
          h2("Welcome to the Survey Explorer"),
          p("This application provides an interactive way to explore survey responses.
            Use the navigation in the sidebar to explore different questions and responses."),
          
          # Statistics section
          div(class = "stats-container",
            div(class = "stat-card",
              h4("Total Responses"),
              div(class = "stat-value", textOutput("total_responses"))
            ),
            div(class = "stat-card",
              h4("Questions"),
              div(class = "stat-value", textOutput("question_count"))
            ),
            div(class = "stat-card",
              h4("Avg. Response Length"),
              div(class = "stat-value", textOutput("avg_response_length"))
            )
          ),
          
          hr(),
          
          # High Priority Insights
          h3("Survey Insights - Demographics & Preferences"),
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
          ),
          
          hr(),
          
          # Course Satisfaction & Discord
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
          ),
          
          hr(),
          
          # Medium Priority Insights
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
          ),
          
          hr(),
          
          # Low Priority Insights
          h3("Section Comparison & Key Themes"),
          p("Comparison across sections and common themes from free text responses."),
          
          div(class = "visualization-container",
            fluidRow(
              column(6,
                div(class = "chart-container",
                  h4("Section Comparison"),
                  girafeOutput("section_comparison_plot", height = "300px")
                )
              ),
              column(6,
                div(class = "chart-container",
                  h4("Key Themes from Free Text"),
                  girafeOutput("key_themes_plot", height = "300px")
                )
              )
            )
          ),
          
          hr(),
          
          # Original Response Insights
          h3("Response Analysis"),
          p("Visual overview of the survey data and response patterns."),
          
          div(class = "visualization-container",
            fluidRow(
              column(6,
                div(class = "chart-container",
                  h4("Response Distribution"),
                  girafeOutput("response_distribution_plot", height = "300px")
                )
              ),
              column(6,
                div(class = "chart-container",
                  h4("Response Length by Question"),
                  girafeOutput("response_length_plot", height = "300px")
                )
              )
            )
          ),
        )
      )
    )
  )
}