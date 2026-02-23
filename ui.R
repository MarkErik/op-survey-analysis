# =============================================================================
# UI.R - Main UI Entry Point for CPSC Experience Survey Explorer
# =============================================================================
# 
# DESCRIPTION:
#   This file defines the main user interface for the Shiny application.
#   It creates a responsive fluidPage layout with custom styling and
#   integrates all tab modules for survey data exploration.
#
# MODULE DEPENDENCIES:
#   - global.R              : Configuration and data loading utilities
#   - ui_components.R       : Reusable UI component functions
#   - home_tab_ui.R         : Home tab with overview visualizations
#   - question_responses_ui.R : Free-text question responses viewer
#   - statistics_tab_ui.R   : Descriptive statistics panel
#   - insights_tab_ui.R     : Advanced statistical analysis
#
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LIBRARY LOADING (using :: namespace operators)
# -----------------------------------------------------------------------------

# Core Shiny framework
# shiny::fluidPage, shiny::tabsetPanel, shiny::tags, etc.

# Dashboard components
# shinydashboard::valueBoxOutput, etc.

# Data table rendering
# DT::dataTableOutput, etc.

# Interactive graphics
# ggiraph::ggiraphOutput, etc.

# HTML utilities
# htmltools::tags, etc.

# -----------------------------------------------------------------------------
# 2. SOURCE MODULE FILES
# -----------------------------------------------------------------------------

source("global.R")              # Load configuration and data utilities
source("ui_components.R")       # Load reusable UI components
source("home_tab_ui.R")         # Load Home tab UI module
source("question_responses_ui.R") # Load Question Responses tab UI module
source("statistics_tab_ui.R")   # Load Statistics tab UI module
source("insights_tab_ui.R")     # Load Insights tab UI module

# -----------------------------------------------------------------------------
# 3. MAIN UI DEFINITION
# -----------------------------------------------------------------------------

ui <- shiny::tagList(
  htmltools::tags$html(
    lang = "en",
    htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    htmltools::tags$title("CPSC Experience Survey Explorer"),
    
    # Custom CSS Styling
    htmltools::tags$head(htmltools::tags$style(type = "text/css", "
      /* === CARD LAYOUTS AND SHADOWS === */
      .visualization-card {
        background: #ffffff;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        padding: 16px;
        margin: 8px;
        transition: box-shadow 0.3s ease;
      }
      .visualization-card:hover {
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.15);
      }
      .total-responses-card, .section-chart-container {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: #ffffff;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      
      /* === BUTTON STYLING === */
      .question-btn {
        background: #f8f9fa;
        border: 2px solid #dee2e6;
        border-radius: 6px;
        padding: 12px 16px;
        margin: 4px;
        cursor: pointer;
        transition: all 0.2s ease;
        font-size: 14px;
      }
      .question-btn:hover, .question-btn.active {
        background: #667eea;
        color: #ffffff;
        border-color: #667eea;
      }
      .btn-reset {
        background: #dc3545;
        color: #ffffff;
        border: none;
        border-radius: 4px;
        padding: 6px 12px;
        cursor: pointer;
      }
      .btn-reset:hover {
        background: #c82333;
      }
      
      /* === VALUEBOX CUSTOMIZATION === */
      .value-box {
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1);
        transition: transform 0.2s ease;
      }
      .value-box:hover {
        transform: translateY(-2px);
      }
      
      /* === SECTION FILTER DISPLAY === */
      .section-filter-display {
        background: #f8f9fa;
        border-radius: 6px;
        padding: 12px 16px;
        margin: 12px 0;
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .section-name {
        font-weight: bold;
        color: #667eea;
      }
      
      /* === RESPONSIVE GRID ADJUSTMENTS === */
      .visualization-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 16px;
        padding: 16px 0;
      }
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
        gap: 12px;
      }
      .question-buttons-row {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
      }
      
      /* === MODAL STYLING === */
      .modal-content {
        border-radius: 8px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
      }
      .modal-header {
        background: #667eea;
        color: #ffffff;
        border-radius: 8px 8px 0 0;
        padding: 16px;
      }
      
      /* === APP HEADER === */
      .app-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: #ffffff;
        padding: 24px;
        border-radius: 0 0 8px 8px;
        margin-bottom: 20px;
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
      }
      .app-title {
        margin: 0 0 8px 0;
        font-size: 28px;
        font-weight: 600;
      }
      .app-subtitle {
        margin: 0;
        opacity: 0.9;
        font-size: 16px;
      }
      
      /* === RESPONSIVE BREAKPOINTS === */
      @media (max-width: 768px) {
        .visualization-grid {
          grid-template-columns: 1fr;
        }
        .stats-grid {
          grid-template-columns: repeat(2, 1fr);
        }
        .question-buttons-row {
          flex-direction: column;
        }
        .question-btn {
          width: 100%;
          text-align: left;
        }
        .app-title {
          font-size: 22px;
        }
      }
      
      /* === ACCESSIBILITY - COLOR CONTRAST === */
      .viz-title {
        color: #333333;
        font-weight: 600;
        margin-bottom: 12px;
      }
      .insight-card {
        background: #ffffff;
        border-left: 4px solid #667eea;
        padding: 16px;
        margin: 8px 0;
      }
      .insight-card.positive {
        border-left-color: #28a745;
      }
      .insight-card.negative {
        border-left-color: #dc3545;
      }
    "))
  ),
  
  # Main Body
  htmltools::tags$body(
    htmltools::tags$div(
      role = "main",
      `aria-label` = "Main application content",
      
      # Application Header
      createAppHeader(),
      
      # Main Tabset Navigation
      htmltools::tags$div(
        class = "main-content",
        role = "navigation",
        `aria-label` = "Main navigation tabs",
        
        shiny::tabsetPanel(
          id = "mainTabset",
          type = "tabs",
          
          # Tab 1: Home - Overview and visualizations
          shiny::tabPanel(
            title = "Home",
            icon = htmltools::tags$i(class = "fa fa-home", `aria-hidden` = "true"),
            homeTabUI("home")
          ),
          
          # Tab 2: Question Responses - Free-text analysis
          shiny::tabPanel(
            title = "Question Responses",
            icon = htmltools::tags$i(class = "fa fa-comments", `aria-hidden` = "true"),
            questionResponsesUI("questionResponses")
          ),
          
          # Tab 3: Statistics - Descriptive statistics
          shiny::tabPanel(
            title = "Statistics",
            icon = htmltools::tags$i(class = "fa fa-chart-bar", `aria-hidden` = "true"),
            statisticsTabUI("statistics")
          ),
          
          # Tab 4: Insights - Advanced analysis
          shiny::tabPanel(
            title = "Insights",
            icon = htmltools::tags$i(class = "fa fa-lightbulb", `aria-hidden` = "true"),
            insightsTabUI("insights")
          )
        )
      )
    )
  )
  )
)