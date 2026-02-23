# =============================================================================
# INSIGHTS_TAB_UI.R - Insights Tab UI Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# UI module for Insights tab - advanced statistical analysis displays

insightsTabUI <- function(id) {
  ns <- NS(id)
  
  htmltools::tags$div(class = "insights-tab", role = "region", `aria-label` = "Statistical insights",
    # 1. Correlation Matrix Section
    htmltools::tags$div(class = "insights-section",
      htmltools::tags$h3("Correlation Analysis"),
      htmltools::tags$div(class = "correlation-container",
        htmltools::tags$div(class = "correlation-heatmap",
          ggiraph::girafeOutput(ns("correlationHeatmap"))
        ),
        htmltools::tags$div(class = "key-insights-panel",
          htmltools::tags$h4("Key Insights"),
          htmltools::tags$div(class = "insight-row",
            htmltools::tags$div(class = "insight-card positive",
              htmltools::tags$h5("Strongest Positive Correlations"),
              htmltools::tags$ul(id = ns("positiveCorrelationsList"))
            ),
            htmltools::tags$div(class = "insight-card negative",
              htmltools::tags$h5("Strongest Negative Correlations"),
              htmltools::tags$ul(id = ns("negativeCorrelationsList"))
            )
          )
        )
      )
    ),
    
    htmltools::tags$hr(),
    
    # 2. Regression Analysis Section
    htmltools::tags$div(class = "insights-section",
      htmltools::tags$h3("Satisfaction Predictors (Regression Analysis)"),
      htmltools::tags$div(class = "regression-container",
        htmltools::tags$div(class = "predictors-table",
          htmltools::tags$table(class = "table table-striped",
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th("Predictor"),
                htmltools::tags$th("Direction"),
                htmltools::tags$th("Coefficient"),
                htmltools::tags$th("Relative Importance")
              )
            ),
            htmltools::tags$tbody(id = ns("predictorsTableBody"))
          )
        )
      )
    ),
    
    htmltools::tags$hr(),
    
    # 3. Student Segmentation Section
    htmltools::tags$div(class = "insights-section",
      htmltools::tags$h3("Student Segmentation"),
      htmltools::tags$div(class = "segmentation-container",
        htmltools::tags$div(class = "segment-viz",
          ggiraph::girafeOutput(ns("clusterProfilesPlot"))
        ),
        htmltools::tags$div(class = "segment-characteristics",
          htmltools::tags$h4("Segment Profiles"),
          htmltools::tags$div(id = ns("segmentProfilesContainer"))
        ),
        htmltools::tags$div(class = "segment-sizes",
          htmltools::tags$h4("Segment Sizes"),
          htmltools::tags$div(id = ns("segmentSizesContainer"))
        )
      )
    ),
    
    htmltools::tags$hr(),
    
    # 4. Section Comparison Section
    htmltools::tags$div(class = "insights-section",
      htmltools::tags$h3("Section Comparison Analysis"),
      htmltools::tags$div(class = "section-comparison-container",
        htmltools::tags$div(id = ns("sectionComparisonResults"))
      )
    ),
    
    htmltools::tags$hr(),
    
    # 5. Effect Size Analysis Section
    htmltools::tags$div(class = "insights-section",
      htmltools::tags$h3("Effect Size Analysis"),
      htmltools::tags$div(class = "effect-size-container",
        htmltools::tags$div(class = "effect-interpretation-guide",
          htmltools::tags$h4("Interpretation Guide"),
          htmltools::tags$ul(
            htmltools::tags$li("Small effect: |d| < 0.5"),
            htmltools::tags$li("Medium effect: 0.5 ≤ |d| < 0.8"),
            htmltools::tags$li("Large effect: |d| ≥ 0.8")
          )
        ),
        htmltools::tags$div(id = ns("effectSizeResults"))
      )
    ),
    
    htmltools::tags$hr(),
    
    # 6. Reliability Analysis Section
    htmltools::tags$div(class = "insights-section",
      htmltools::tags$h3("Reliability Analysis (Cronbach's Alpha)"),
      htmltools::tags$div(class = "reliability-container",
        htmltools::tags$div(class = "reliability-grid",
          htmltools::tags$div(id = ns("reliabilityResults"))
        )
      )
    )
  )
}