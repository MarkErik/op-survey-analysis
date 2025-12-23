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
          
          # Data visualization section
          h3("Survey Insights"),
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