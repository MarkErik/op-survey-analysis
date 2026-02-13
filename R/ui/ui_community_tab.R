# Community & Belonging Tab UI
# Social aspects, community engagement, and Discord usage analysis

#' Create Community Tab
#'
#' Creates the Community & Belonging tab with 5 belonging statements visualizations
#' and Discord engagement section.
#'
#' @return A shiny UI element for the Community & Belonging tab
#' @export
create_community_tab <- function() {
  tabPanel(
    title = "Community & Belonging",
    value = "community",
    icon = icon("users"),
    div(
      class = "tab-content community-tab",
      # Page Header
      div(
        class = "page-header",
        h2("Community & Belonging Analysis", class = "page-title"),
        p("Analysis of community engagement, belonging statements, and Discord usage", class = "page-subtitle")
      ),
      
      # Comparison Controls
      div(
        class = "comparison-section",
        create_comparison_controls(id = "community_comparison")
      ),
      
      # Belonging Statements Section
      div(
        class = "belonging-section",
        h3("Belonging Statements", class = "section-title"),
        p("Responses to 5 Likert-scale statements about community belonging", class = "section-description"),
        
        # Overall Belonging Score
        fluidRow(
          column(4,
            create_stat_card(
              title = "Overall Belonging Score",
              value = uiOutput("community_belonging_score"),
              subtitle = "Average across all statements",
              icon = "heart"
            )
          ),
          column(4,
            create_stat_card(
              title = "Strong Agreement Rate",
              value = uiOutput("community_strong_agreement_rate"),
              subtitle = "Percentage of 'Strongly Agree' responses",
              icon = "thumbs-up"
            )
          ),
          column(4,
            create_stat_card(
              title = "Discord Engagement",
              value = uiOutput("community_discord_engagement"),
              subtitle = "Students using Discord",
              icon = "comments"
            )
          )
        ),
        
        # Belonging Statements Distribution
        fluidRow(
          column(12,
            create_chart_container(
              title = "Belonging Statements Distribution",
              plot_id = "belonging_statements_distribution",
              ns = NS("community")
            )
          )
        ),
        
        # Individual Statement Charts
        fluidRow(
          column(6,
            create_chart_container(
              title = uiOutput("belonging_statement_1_title"),
              plot_id = "belonging_statement_1",
              ns = NS("community")
            )
          ),
          column(6,
            create_chart_container(
              title = uiOutput("belonging_statement_2_title"),
              plot_id = "belonging_statement_2",
              ns = NS("community")
            )
          )
        ),
        fluidRow(
          column(6,
            create_chart_container(
              title = uiOutput("belonging_statement_3_title"),
              plot_id = "belonging_statement_3",
              ns = NS("community")
            )
          ),
          column(6,
            create_chart_container(
              title = uiOutput("belonging_statement_4_title"),
              plot_id = "belonging_statement_4",
              ns = NS("community")
            )
          )
        ),
        fluidRow(
          column(12,
            create_chart_container(
              title = uiOutput("belonging_statement_5_title"),
              plot_id = "belonging_statement_5",
              ns = NS("community")
            )
          )
        )
      ),
      
      # Discord Engagement Section
      div(
        class = "discord-section",
        h3("Discord Engagement", class = "section-title"),
        p("Analysis of Discord server usage and feature engagement", class = "section-description"),
        
        fluidRow(
          # Discord Feature Usage
          column(6,
            create_chart_container(
              title = "Discord Feature Usage",
              plot_id = "discord_feature_usage",
              ns = NS("community")
            )
          ),
          # Discord Usage Patterns
          column(6,
            create_chart_container(
              title = "Discord Usage Patterns",
              plot_id = "discord_usage_patterns",
              ns = NS("community")
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
              title = "Belonging Scores by Section",
              plot_id = "belonging_section_comparison",
              ns = NS("community")
            )
          )
        ),
        fluidRow(
          column(12,
            create_chart_container(
              title = "Discord Usage by Section",
              plot_id = "discord_section_comparison",
              ns = NS("community")
            )
          )
        )
      ),
      
      # Experience-Based Comparisons
      div(
        class = "experience-section",
        h3("Experience-Based Analysis", class = "section-title"),
        fluidRow(
          column(12,
            create_chart_container(
              title = "Belonging by Experience Level",
              plot_id = "belonging_experience_comparison",
              ns = NS("community")
            )
          )
        )
      ),
      
      # Insights Panel
      div(
        class = "insights-section",
        create_insights_panel(id = "community_insights", title = "Community & Belonging Insights")
      )
    )
  )
}
