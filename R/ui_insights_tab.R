# Insights Tab UI - Advanced Statistical Analysis
# Contains the UI elements for advanced statistical insights and correlations
#
# @author Course Instructor
# @version 2.0.0

#' Insights Tab UI
#'
#' Creates the complete Insights tab interface with
#' correlation matrix, regression analysis, cluster analysis, and section comparison
#'
#' @return UI element
#' @export
ui_insights_tab <- function() {
  div(
    class = "main-content",

    # Page header
    div(
      class = "page-header",
      h1("Insights", class = "page-title"),
      p("Advanced statistical analysis and patterns in survey data", class = "page-subtitle")
    ),

    # Section filter display
    uiOutput(outputId = "insights_section_filter_display"),

    # Category selector for insights
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-primary text-white",
        h3("Analysis Category", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Select a category to analyze:", class = "text-muted mb-3"),
        navs_pill(
          id = "insights_category",
          selected = "course_satisfaction",
          nav_item(
            a(
              href = "#",
              onclick = "Shiny.setInputValue('insights_category', 'course_satisfaction'); return false;",
              icon("star"), " Course Satisfaction"
            )
          ),
          nav_item(
            a(
              href = "#",
              onclick = "Shiny.setInputValue('insights_category', 'learning_methods'); return false;",
              icon("book"), " Learning Methods"
            )
          ),
          nav_item(
            a(
              href = "#",
              onclick = "Shiny.setInputValue('insights_category', 'community_belonging'); return false;",
              icon("users"), " Community & Belonging"
            )
          )
        )
      )
    ),

    # Correlation Analysis Section
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-info text-white",
        h3(icon("project-diagram"), " Correlation Analysis", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Explore relationships between different survey questions. Green indicates positive correlation, red indicates negative correlation.",
          class = "text-muted"),
        girafeOutput(outputId = "insights_correlation_matrix", width = "100%", height = "600px"),
        hr(),
        h4("Key Insights"),
        uiOutput(outputId = "insights_correlation_insights")
      )
    ),

    # Regression Analysis Section
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-success text-white",
        h3(icon("chart-line"), " Regression Analysis", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Identify which factors most strongly predict overall satisfaction.",
          class = "text-muted"),
        fluidRow(
          column(
            width = 6,
            h4("Satisfaction Predictors"),
            plotOutput(outputId = "insights_regression_plot", height = "400px")
          ),
          column(
            width = 6,
            h4("Top Predictors Table"),
            DT::dataTableOutput(outputId = "insights_regression_table")
          )
        )
      )
    ),

    # Cluster Analysis Section
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-warning text-dark",
        h3(icon("object-group"), " Student Segmentation", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Automatic grouping of students based on their response patterns.",
          class = "text-muted"),
        fluidRow(
          column(
            width = 6,
            h4("Cluster Distribution"),
            plotOutput(outputId = "insights_cluster_plot", height = "300px")
          ),
          column(
            width = 6,
            h4("Cluster Profiles"),
            uiOutput(outputId = "insights_cluster_profiles")
          )
        ),
        hr(),
        h4("Cluster Characteristics"),
        DT::dataTableOutput(outputId = "insights_cluster_table")
      )
    ),

    # Section Comparison Analysis
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-danger text-white",
        h3(icon("balance-scale"), " Section Comparison", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Formal statistical tests comparing responses across sections.",
          class = "text-muted"),
        fluidRow(
          column(
            width = 8,
            plotOutput(outputId = "insights_section_comparison_plot", height = "400px")
          ),
          column(
            width = 4,
            h4("ANOVA Results"),
            uiOutput(outputId = "insights_anova_results")
          )
        ),
        hr(),
        h4("Effect Sizes"),
        p("Practical significance of differences between sections.",
          class = "text-muted"),
        DT::dataTableOutput(outputId = "insights_effect_sizes")
      )
    ),

    # Reliability Analysis Section
    div(
      class = "card-custom mb-4",
      div(
        class = "card-header bg-secondary text-white",
        h3(icon("check-circle"), " Reliability Analysis", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Measure how consistently students answered related questions.",
          class = "text-muted"),
        fluidRow(
          column(
            width = 6,
            h4("Cronbach's Alpha by Category"),
            plotOutput(outputId = "insights_reliability_plot", height = "300px")
          ),
          column(
            width = 6,
            h4("Reliability Scores"),
            DT::dataTableOutput(outputId = "insights_reliability_table")
          )
        )
      )
    ),

    # Interaction Analysis Section
    div(
      class = "card-custom",
      div(
        class = "card-header bg-dark text-white",
        h3(icon("exchange-alt"), " Interaction Analysis", class = "mb-0")
      ),
      div(
        class = "card-body",
        p("Explore how different factors combine to affect outcomes.",
          class = "text-muted"),
        fluidRow(
          column(
            width = 12,
            h4("Significant Interactions"),
            uiOutput(outputId = "insights_interactions")
          )
        )
      )
    )
  )
}

#' Create correlation heatmap UI
#'
#' Generates the UI for correlation matrix visualization
#'
#' @return girafeOutput element
#' @export
ui_correlation_heatmap <- function() {
  girafeOutput(
    outputId = "insights_correlation_heatmap",
    width = "100%",
    height = "600px"
  )
}

#' Create regression plot UI
#'
#' Generates the UI for regression visualization
#'
#' @return plotOutput element
#' @export
ui_regression_plot <- function() {
  plotOutput(
    outputId = "insights_regression_plot",
    height = "400px"
  )
}

#' Create cluster plot UI
#'
#' Generates the UI for cluster visualization
#'
#' @return plotOutput element
#' @export
ui_cluster_plot <- function() {
  plotOutput(
    outputId = "insights_cluster_plot",
    height = "300px"
  )
}

#' Create section comparison plot UI
#'
#' Generates the UI for section comparison visualization
#'
#' @return plotOutput element
#' @export
ui_insights_section_comparison <- function() {
  plotOutput(
    outputId = "insights_section_comparison_plot",
    height = "400px"
  )
}

#' Create reliability plot UI
#'
#' Generates the UI for reliability visualization
#'
#' @return plotOutput element
#' @export
ui_reliability_plot <- function() {
  plotOutput(
    outputId = "insights_reliability_plot",
    height = "300px"
  )
}

#' Display correlation insights
#'
#' Generates UI showing strongest correlations
#'
#' @param correlations Data frame of correlations
#' @return UI element
#' @export
ui_correlation_insights <- function(correlations) {
  if (is.null(correlations) || nrow(correlations) == 0) {
    return(div(class = "alert alert-info", "No significant correlations found"))
  }

  tagList(
    div(
      class = "alert alert-success",
      h5(icon("arrow-up"), " Strongest Positive Correlations"),
      lapply(seq_len(min(3, nrow(correlations %>% dplyr::filter(correlation > 0)))), function(i) {
        row <- correlations %>% dplyr::filter(correlation > 0) %>% dplyr::arrange(dplyr::desc(correlation)) %>% dplyr::slice(i)
        p(paste0(row$var1, " ↔ ", row$var2, ": ", round(row$correlation, 3)))
      })
    ),
    div(
      class = "alert alert-danger",
      h5(icon("arrow-down"), " Strongest Negative Correlations"),
      lapply(seq_len(min(3, nrow(correlations %>% dplyr::filter(correlation < 0)))), function(i) {
        row <- correlations %>% dplyr::filter(correlation < 0) %>% dplyr::arrange(correlation) %>% dplyr::slice(i)
        p(paste0(row$var1, " ↔ ", row$var2, ": ", round(row$correlation, 3)))
      })
    )
  )
}

#' Display cluster profiles
#'
#' Generates UI showing characteristics of each cluster
#'
#' @param clusters Data frame of cluster profiles
#' @return UI element
#' @export
ui_cluster_profiles <- function(clusters) {
  if (is.null(clusters) || nrow(clusters) == 0) {
    return(div(class = "alert alert-info", "No clusters identified"))
  }

  profiles <- lapply(seq_len(nrow(clusters)), function(i) {
    cluster <- clusters[i, ]
    div(
      class = "card mb-2",
      div(class = "card-body",
          h5(paste0("Cluster ", cluster$cluster_id, " (", cluster$percentage, "%)")),
          p(cluster$description, class = "text-muted")
      )
    )
  })

  tagList(profiles)
}

#' Display ANOVA results
#'
#' Generates UI showing ANOVA test results
#'
#' @param anova_results List of ANOVA results
#' @return UI element
#' @export
ui_anova_results <- function(anova_results) {
  if (is.null(anova_results)) {
    return(div(class = "alert alert-info", "No ANOVA results available"))
  }

  tagList(
    div(
      class = "table-responsive",
      table(class = "table table-sm",
            tr(th("F-statistic"), td(round(anova_results$f_statistic, 3))),
            tr(th("p-value"), td(sprintf("%.4f", anova_results$p_value))),
            tr(th("Significant"), td(ifelse(anova_results$p_value < 0.05, "Yes", "No")))
      )
    )
  )
}

#' Display interaction effects
#'
#' Generates UI showing significant interaction effects
#'
#' @param interactions Data frame of interaction effects
#' @return UI element
#' @export
ui_interaction_effects <- function(interactions) {
  if (is.null(interactions) || nrow(interactions) == 0) {
    return(div(class = "alert alert-info", "No significant interactions found"))
  }

  div(
    class = "table-responsive",
    DT::datatable(
      interactions,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  )
}
