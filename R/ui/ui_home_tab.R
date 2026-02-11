# UI Home Tab Component

# Create the home tab content
create_home_tab <- function() {
  tabPanel("Home",
    fluidRow(
      column(12,
        div(class = "home-content",
          # Responses section
          div(class = "stats-section",
            h3("Responses"),
            p("Select a course section to filter data below"),
            
            # First row: Section chart and stat cards
            div(class = "overview-first-row",
              fluidRow(
                column(8,
                  div(class = "chart-container",
                    h4("Responses per Section"),
                    girafeOutput("section_breakdown_plot", height = "300px")
                  )
                ),
                column(4,
                  div(class = "overview-stats-column",
                    div(class = "stat-card",
                      h4("Total Responses"),
                      div(class = "stat-value", textOutput("total_responses"))
                    ),
                    div(class = "stat-card",
                      h4("Selected Section"),
                      div(class = "stat-value", textOutput("selected_section_display")),
                      uiOutput("reset_section_filter_ui")
                    )
                  )
                )
              )
            )
          ),
          
          # Overview section
          div(class = "insights-section",
            h3("Overview"),
            
            div(class = "visualization-container",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    uiOutput("learning_preference_title"),
                    girafeOutput("learning_preference_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    uiOutput("prior_experience_title"),
                    girafeOutput("prior_experience_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    uiOutput("course_satisfaction_title"),
                    girafeOutput("course_satisfaction_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    uiOutput("discord_engagement_title"),
                    girafeOutput("discord_engagement_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    uiOutput("learning_methods_title"),
                    girafeOutput("learning_methods_plot", height = "300px")
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    uiOutput("community_connection_title"),
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
