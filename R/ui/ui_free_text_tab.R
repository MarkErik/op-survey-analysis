# Free Text Responses Tab UI
# Browse all open-ended responses with question selector and participant profiles

#' Create Free Text Tab
#'
#' Creates the Free Text Responses tab with question selector, response browser,
#' and participant profile modal trigger.
#'
#' @param free_text_questions A character vector of free text question column names
#' @return A shiny UI element for the Free Text Responses tab
#' @export
create_free_text_tab <- function(free_text_questions) {
  tabPanel(
    title = "Free Text Responses",
    value = "free_text",
    div(
      class = "tab-content free-text-tab",
      # Page Header
      div(
        class = "page-header",
        h2("Free Text Responses", class = "page-title"),
        p("Browse all open-ended responses and participant profiles", class = "page-subtitle")
      ),
      
      # Question Selector Section
      div(
        class = "question-selector-section",
        h3("Select Question", class = "section-title"),
        fluidRow(
          column(12,
            div(
              class = "question-selector",
              selectInput(
                "free_text_question",
                label = "Choose a question to view responses:",
                choices = free_text_questions,
                selected = free_text_questions[1],
                width = "100%"
              )
            )
          )
        )
      ),
      
      # Response Statistics
      div(
        class = "response-stats-section",
        fluidRow(
          column(4,
            create_stat_card(
              title = "Total Responses",
              value = uiOutput("free_text_total_responses"),
              subtitle = "Responses for selected question"
            )
          ),
          column(4,
            create_stat_card(
              title = "Response Rate",
              value = uiOutput("free_text_response_rate"),
              subtitle = "Percentage of participants"
            )
          ),
          column(4,
            create_stat_card(
              title = "Avg Word Count",
              value = uiOutput("free_text_avg_words"),
              subtitle = "Average response length"
            )
          )
        )
      ),
      
      # Response Browser Section
      div(
        class = "response-browser-section",
        h3("Response Browser", class = "section-title"),
        
        # Filter Controls
        div(
          class = "browser-controls",
          fluidRow(
            column(4,
              selectInput(
                "free_text_section_filter",
                label = "Filter by Section:",
                choices = c("All Sections" = "all"),
                selected = "all",
                multiple = FALSE
              )
            ),
            column(4,
              selectInput(
                "free_text_experience_filter",
                label = "Filter by Experience:",
                choices = c(
                  "All Levels" = "all",
                  "No Experience" = "none",
                  "Some Experience" = "some",
                  "Highly Experienced" = "high"
                ),
                selected = "all",
                multiple = FALSE
              )
            ),
            column(4,
              textInput(
                "free_text_search",
                label = "Search responses:",
                placeholder = "Enter keywords...",
                width = "100%"
              )
            )
          )
        ),
        
        # Response List
        div(
          class = "response-list",
          uiOutput("free_text_response_list")
        )
      ),
      
      # All Questions Summary
      div(
        class = "all-questions-section",
        h3("All Questions Summary", class = "section-title"),
        p("Overview of response counts for all free text questions", class = "section-description"),
        
        fluidRow(
          column(12,
            create_chart_container(
              title = "Response Count by Question",
              plot_id = "free_text_question_counts",
              ns = NS("free_text")
            )
          )
        )
      ),
      
      # Participant Profile Modal Trigger
      div(
        class = "participant-profile-section",
        h3("Participant Profiles", class = "section-title"),
        p("View complete survey responses for individual participants", class = "section-description"),
        
        fluidRow(
          column(6,
            div(
              class = "participant-selector",
              selectInput(
                "participant_selector",
                label = "Select a participant:",
                choices = c("Select a participant..." = ""),
                selected = "",
                width = "100%"
              )
            )
          ),
          column(6,
            div(
              class = "participant-actions",
              actionButton(
                "view_participant_profile",
                "View Full Profile",
                class = "btn-primary"
              )
            )
          )
        )
      ),
      
      # Insights Panel
      div(
        class = "insights-section",
        create_insights_panel(id = "free_text_insights", title = "Free Text Insights")
      )
    )
  )
}
