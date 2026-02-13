# Learning Elements Tab UI
# Learning method effectiveness analysis with 11 learning elements

#' Create Learning Elements Tab
#'
#' Creates the Learning Elements tab with 11 learning elements contribution rankings
#' and comparison features.
#'
#' @return A shiny UI element for the Learning Elements tab
#' @export
create_learning_elements_tab <- function() {
  tabPanel(
    title = "Learning Elements",
    value = "learning_elements",
    div(
      class = "tab-content learning-elements-tab",
      # Page Header
      div(
        class = "page-header",
        h2("Learning Elements Analysis", class = "page-title"),
        p("Analysis of 11 learning elements and their contribution to student learning", class = "page-subtitle")
      ),
      
      # Comparison Controls
      div(
        class = "comparison-section",
        create_comparison_controls(id = "learning_elements_comparison")
      ),
      
      # Learning Contribution Rankings Section
      div(
        class = "rankings-section",
        h3("Learning Contribution Rankings", class = "section-title"),
        p("Average contribution ratings for each learning element", class = "section-description"),
        
        # Overall Rankings
        fluidRow(
          column(12,
            create_chart_container(
              title = "Element Rankings (Average Contribution)",
              plot_id = "learning_elements_rankings",
              ns = NS("learning_elements")
            )
          )
        ),
        
        # Element Distribution
        fluidRow(
          column(12,
            create_chart_container(
              title = "Element Contribution Distribution",
              plot_id = "learning_elements_distribution",
              ns = NS("learning_elements")
            )
          )
        )
      ),
      
      # Individual Element Charts
      div(
        class = "elements-detail-section",
        h3("Element Details", class = "section-title"),
        p("Detailed breakdown for each learning element", class = "section-description"),
        
        fluidRow(
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_1_title"),
              plot_id = "learning_element_1",
              ns = NS("learning_elements")
            )
          ),
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_2_title"),
              plot_id = "learning_element_2",
              ns = NS("learning_elements")
            )
          ),
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_3_title"),
              plot_id = "learning_element_3",
              ns = NS("learning_elements")
            )
          )
        ),
        fluidRow(
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_4_title"),
              plot_id = "learning_element_4",
              ns = NS("learning_elements")
            )
          ),
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_5_title"),
              plot_id = "learning_element_5",
              ns = NS("learning_elements")
            )
          ),
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_6_title"),
              plot_id = "learning_element_6",
              ns = NS("learning_elements")
            )
          )
        ),
        fluidRow(
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_7_title"),
              plot_id = "learning_element_7",
              ns = NS("learning_elements")
            )
          ),
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_8_title"),
              plot_id = "learning_element_8",
              ns = NS("learning_elements")
            )
          ),
          column(4,
            create_chart_container(
              title = uiOutput("learning_element_9_title"),
              plot_id = "learning_element_9",
              ns = NS("learning_elements")
            )
          )
        ),
        fluidRow(
          column(6,
            create_chart_container(
              title = uiOutput("learning_element_10_title"),
              plot_id = "learning_element_10",
              ns = NS("learning_elements")
            )
          ),
          column(6,
            create_chart_container(
              title = uiOutput("learning_element_11_title"),
              plot_id = "learning_element_11",
              ns = NS("learning_elements")
            )
          )
        )
      ),
      
      # Correlation Analysis
      div(
        class = "correlation-section",
        h3("Correlation Analysis", class = "section-title"),
        p("Relationships between different learning elements", class = "section-description"),
        
        fluidRow(
          column(12,
            create_chart_container(
              title = "Learning Elements Correlation Matrix",
              plot_id = "learning_elements_correlation",
              ns = NS("learning_elements")
            )
          )
        )
      ),
      
      # Experience-Based Comparisons
      div(
        class = "experience-section",
        h3("Experience-Based Analysis", class = "section-title"),
        p("How learning element ratings vary by programming experience", class = "section-description"),
        
        fluidRow(
          column(12,
            create_chart_container(
              title = "Element Ratings by Experience Level",
              plot_id = "learning_elements_experience_comparison",
              ns = NS("learning_elements")
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
              title = "Element Ratings by Section",
              plot_id = "learning_elements_section_comparison",
              ns = NS("learning_elements")
            )
          )
        )
      ),
      
      # Insights Panel
      div(
        class = "insights-section",
        create_insights_panel(id = "learning_elements_insights", title = "Learning Elements Insights")
      )
    )
  )
}
