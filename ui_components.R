# UI Components for CPSC Experience Survey Explorer - Reusable UI building blocks

# === HEADER COMPONENTS ===

createAppHeader <- function() {
  htmltools::tags$div(class = "app-header",
    htmltools::tags$h1(class = "app-title", "CPSC Experience Survey Explorer", aria_label = "Application title"),
    htmltools::tags$p(class = "app-subtitle", "Analyze student feedback and course experience data")
  )
}

createSectionFilterDisplay <- function() {
  htmltools::tags$div(class = "section-filter-display", role = "region", `aria-label` = "Section filter status",
    htmltools::tags$span(class = "filter-label", "Selected Section: "),
    htmltools::tags$span(id = "selectedSectionName", class = "section-name", "(All Sections)"),
    htmltools::tags$button(id = "resetFilterBtn", class = "btn-reset", "Reset Filter", `aria-label` = "Reset section filter")
  )
}

# === NAVIGATION COMPONENTS ===

createMainTabset <- function(...) {
  htmltools::tags$div(class = "main-tabset-container",
    shiny::tabsetPanel(id = "mainTabset", type = "tabs",
      shiny::tabPanel("Home", createHomeTabContent()),
      shiny::tabPanel("Question Responses", createQuestionResponsesTabContent()),
      shiny::tabPanel("Statistics", createStatisticsTabContent()),
      shiny::tabPanel("Insights", createInsightsTabContent()),
      ...
    )
  )
}

# === HOME TAB COMPONENTS ===

createHomeTabContent <- function() {
  htmltools::tags$div(class = "home-tab-content",
    createResponseOverviewSection(), htmltools::tags$hr(), createVisualizationGrid()
  )
}

createResponseOverviewSection <- function() {
  htmltools::tags$div(class = "response-overview-section", role = "region", `aria-label` = "Response overview",
    htmltools::tags$div(class = "overview-row",
      htmltools::tags$div(class = "total-responses-card",
        htmltools::tags$h3("Total Responses"),
        htmltools::tags$div(id = "totalResponsesCount", class = "response-count", "0")
      ),
      htmltools::tags$div(class = "section-chart-container",
        htmltools::tags$h4("Responses per Section"),
        shiny::plotOutput("sectionBreakdownChart", height = "250px")
      )
    ),
    createSectionFilterDisplay()
  )
}

createVisualizationGrid <- function() {
  htmltools::tags$div(class = "visualization-grid", role = "region", `aria-label` = "Overview visualizations",
    createVisualizationCard("learningPreferencePlot", "Learning Preference Distribution"),
    createVisualizationCard("priorExperiencePlot", "Prior Programming Experience"),
    createVisualizationCard("satisfactionOverviewPlot", "Course Satisfaction Overview"),
    createVisualizationCard("discordEngagementPlot", "Discord Engagement Metrics"),
    createVisualizationCard("learningMethodsPlot", "Most Valuable Learning Methods"),
    createVisualizationCard("communityConnectionPlot", "Community Connection Scores")
  )
}

createVisualizationCard <- function(plotId, title) {
  htmltools::tags$div(class = "visualization-card",
    htmltools::tags$h4(class = "viz-title", title),
    htmltools::tags$div(class = "viz-plot-container", shiny::plotOutput(plotId, height = "200px"))
  )
}

# === QUESTION RESPONSES TAB COMPONENTS ===

createQuestionResponsesTabContent <- function() {
  htmltools::tags$div(class = "question-responses-tab",
    createQuestionSelectorButtons(), htmltools::tags$hr(), createResponsesTable()
  )
}

createQuestionSelectorButtons <- function() {
  htmltools::tags$div(class = "question-selector-container", role = "group", `aria-label` = "Select question",
    htmltools::tags$h3("Select a Question"),
    htmltools::tags$div(class = "question-buttons-row",
      shiny::actionButton("q1_btn", "What helps you learn?"),
      shiny::actionButton("q2_btn", "What hinders your learning?"),
      shiny::actionButton("q3_btn", "Suggestions for improvement"),
      shiny::actionButton("q4_btn", "Best aspect of the course"),
      shiny::actionButton("q5_btn", "Worst aspect of the course"),
      shiny::actionButton("q6_btn", "Additional comments")
    )
  )
}

createResponsesTable <- function() {
  htmltools::tags$div(class = "responses-table-container", role = "region", `aria-label` = "Survey responses table",
    htmltools::tags$h3(id = "selectedQuestionTitle", "Select a question to view responses"),
    htmltools::tags$div(id = "responsesTableWrapper", class = "dt-responsive", DT::DTOutput("responsesTable"))
  )
}

createParticipantModal <- function() {
  shiny::modalDialog(title = "Participant Profile", size = "l", easyClose = TRUE,
    htmltools::tags$div(class = "participant-modal", role = "dialog", `aria-label` = "Participant profile",
      htmltools::tags$div(class = "participant-basic-info",
        htmltools::tags$h4("Basic Information"),
        htmltools::tags$div(class = "info-grid",
          htmltools::tags$p(htmltools::tags$strong("Section: "), htmltools::tags$span(id = "modalSection")),
          htmltools::tags$p(htmltools::tags$strong("Prior Experience: "), htmltools::tags$span(id = "modalExperience")),
          htmltools::tags$p(htmltools::tags$strong("Learning Preference: "), htmltools::tags$span(id = "modalPreference"))
        )
      ),
      htmltools::tags$hr(),
      htmltools::tags$div(class = "participant-selected-response",
        htmltools::tags$h4("Selected Response"),
        htmltools::tags$p(id = "modalSelectedResponse", class = "response-text")
      ),
      htmltools::tags$hr(),
      htmltools::tags$div(class = "participant-all-responses",
        htmltools::tags$h4("All Other Responses"),
        htmltools::tags$div(id = "modalOtherResponses", class = "other-responses-list")
      )
    )
  )
}

# === STATISTICS TAB COMPONENTS ===

createStatisticsTabContent <- function() {
  htmltools::tags$div(class = "statistics-tab",
    createCategorySelector(), htmltools::tags$hr(),
    createLikertQuestionButtons(), htmltools::tags$hr(),
    htmltools::tags$div(class = "statistics-results",
      createStatisticalSummaryPanel(), createDistributionHistogram()
    )
  )
}

createCategorySelector <- function() {
  htmltools::tags$div(class = "category-selector-container", role = "group", `aria-label` = "Select category",
    htmltools::tags$h3("Question Category"),
    shiny::selectInput("likertCategory", label = NULL,
      choices = c("Course Satisfaction" = "course_satisfaction", "Learning Methods" = "learning_methods", "Community & Belonging" = "community_belonging"),
      selected = "course_satisfaction"
    )
  )
}

createLikertQuestionButtons <- function() {
  htmltools::tags$div(class = "likert-buttons-container", role = "group", `aria-label` = "Select question",
    htmltools::tags$h3("Select a Question"),
    htmltools::tags$div(id = "likertButtonsWrapper", class = "likert-buttons-row")
  )
}

createStatisticalSummaryPanel <- function() {
  htmltools::tags$div(class = "statistical-summary-panel", role = "region", `aria-label` = "Statistical summary",
    htmltools::tags$h4("Descriptive Statistics"),
    htmltools::tags$div(id = "statisticsSummary", class = "stats-grid",
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "N: "), htmltools::tags$span(id = "statN")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Mean: "), htmltools::tags$span(id = "statMean")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Median: "), htmltools::tags$span(id = "statMedian")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Mode: "), htmltools::tags$span(id = "statMode")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "SD: "), htmltools::tags$span(id = "statSD")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "SE: "), htmltools::tags$span(id = "statSE")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Min: "), htmltools::tags$span(id = "statMin")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Max: "), htmltools::tags$span(id = "statMax")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Q1: "), htmltools::tags$span(id = "statQ1")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Q3: "), htmltools::tags$span(id = "statQ3")),
      htmltools::tags$div(class = "stat-item", htmltools::tags$span(class = "stat-label", "Missing: "), htmltools::tags$span(id = "statMissing"))
    )
  )
}

createDistributionHistogram <- function() {
  htmltools::tags$div(class = "distribution-histogram-container", role = "region", `aria-label` = "Response distribution",
    htmltools::tags$h4("Response Distribution"),
    shiny::plotOutput("likertHistogram", height = "300px")
  )
}

# === INSIGHTS TAB COMPONENTS ===

createInsightsTabContent <- function() {
  htmltools::tags$div(class = "insights-tab",
    htmltools::tags$h2("Advanced Statistical Insights"),
    createCorrelationMatrixView(), htmltools::tags$hr(),
    createRegressionAnalysisPanel(), htmltools::tags$hr(),
    createSegmentationView(), htmltools::tags$hr(),
    createEffectSizePanel()
  )
}

createCorrelationMatrixView <- function() {
  htmltools::tags$div(class = "correlation-matrix-view", role = "region", `aria-label` = "Correlation analysis",
    htmltools::tags$h3("Correlation Matrix"),
    htmltools::tags$p(class = "insight-description", "Visual representation of relationships between Likert-scale questions."),
    htmltools::tags$div(class = "correlation-container",
      shiny::plotOutput("correlationHeatmap", height = "500px"),
      htmltools::tags$div(id = "correlationInsights", class = "correlation-insights",
        htmltools::tags$h4("Key Correlations"),
        htmltools::tags$div(id = "strongPositiveCorrelations"),
        htmltools::tags$div(id = "strongNegativeCorrelations")
      )
    )
  )
}

createRegressionAnalysisPanel <- function() {
  htmltools::tags$div(class = "regression-analysis-panel", role = "region", `aria-label` = "Regression analysis",
    htmltools::tags$h3("Regression Analysis"),
    htmltools::tags$p(class = "insight-description", "Factors that best predict overall course satisfaction"),
    htmltools::tags$div(class = "regression-results",
      htmltools::tags$div(class = "predictor-ranking",
        htmltools::tags$h4("Predictor Variables (Ranked by Influence)"),
        htmltools::tags$div(id = "predictorRankingTable")
      ),
      shiny::plotOutput("regressionPlot", height = "300px")
    )
  )
}

createSegmentationView <- function() {
  htmltools::tags$div(class = "segmentation-view", role = "region", `aria-label` = "Student segmentation",
    htmltools::tags$h3("Student Segmentation"),
    htmltools::tags$p(class = "insight-description", "Automatic grouping of students based on response patterns"),
    htmltools::tags$div(class = "segmentation-container",
      htmltools::tags$div(class = "cluster-summary",
        htmltools::tags$h4(id = "clusterCount"),
        htmltools::tags$div(id = "clusterDescriptions")
      ),
      shiny::plotOutput("clusterProfilePlot", height = "400px"),
      htmltools::tags$div(class = "cluster-details",
        htmltools::tags$h4("Segment Characteristics"),
        htmltools::tags$div(id = "clusterCharacteristics")
      )
    )
  )
}

createEffectSizePanel <- function() {
  htmltools::tags$div(class = "effect-size-panel", role = "region", `aria-label` = "Effect size analysis",
    htmltools::tags$h3("Effect Size Analysis"),
    htmltools::tags$p(class = "insight-description", "Practical significance of differences between sections"),
    htmltools::tags$div(class = "effect-size-results",
      htmltools::tags$div(class = "effect-size-summary",
        htmltools::tags$h4("Largest Effect Sizes"),
        htmltools::tags$div(id = "effectSizeTable")
      ),
      shiny::plotOutput("effectSizePlot", height = "300px"),
      htmltools::tags$div(class = "effect-interpretation",
        htmltools::tags$h4("Interpretation Guide"),
        htmltools::tags$ul(
          htmltools::tags$li("Small effect: d = 0.2"),
          htmltools::tags$li("Medium effect: d = 0.5"),
          htmltools::tags$li("Large effect: d = 0.8+")
        )
      )
    )
  )
}