# R/app_config.R
# Application configuration variables for the CPSC Experience Survey Explorer

# =============================================================================
# Application Metadata
# =============================================================================

APP_TITLE <- "CPSC Experience Survey Explorer"
APP_VERSION <- "1.0.0"
APP_DESCRIPTION <- "Interactive survey analysis tool for course instructors"

# =============================================================================
# Data File Configuration
# =============================================================================

DATA_FILE_PATH <- "survey_data/CPSC Experience Survey.csv"
DATA_ENCODING <- "UTF-8"

# =============================================================================
# Visualization Settings
# =============================================================================

PLOT_WIDTH <- 800
PLOT_HEIGHT <- 400
PLOT_ASPECT_RATIO <- PLOT_WIDTH / PLOT_HEIGHT

# =============================================================================
# Table Settings
# =============================================================================

TABLE_PAGE_SIZE <- 30
TABLE_SHOW_ALL_ROWS <- FALSE
TABLE_SCROLLX <- TRUE
TABLE_SCROLLY <- TRUE

# =============================================================================
# Animation Settings
# =============================================================================

ANIMATION_DURATION <- 300
ANIMATION_DELAY <- 100

# =============================================================================
# Likert Scale Settings
# =============================================================================

LIKERT_MIN_VALUE <- 1
LIKERT_MAX_VALUE <- 5
LIKERT_LABELS <- c(
  "1 - Strongly Disagree",
  "2",
  "3",
  "4",
  "5 - Strongly Agree"
)

# =============================================================================
# Discord Options for Multi-Select Parsing
# =============================================================================

DISCORD_OPTIONS <- c(
  "I have joined the class Discord",
  "I am active in the class Discord",
  "It is really useful for me for learning",
  "I use it for announcements",
  "I use it for social interaction",
  "I use it for asking questions",
  "I use it for sharing resources"
)

# =============================================================================
# Section Display Settings
# =============================================================================

SECTION_DISPLAY_FORMAT <- "{course} - {time}"
SECTION_SORT_ORDER <- c("231 - 1pm", "231 - 11am", "231 - 3pm", "217 - 1pm", "217 - 11am", "217 - 3pm")

# =============================================================================
# Default Filter Settings
# =============================================================================

DEFAULT_SECTION_FILTER <- NULL  # NULL means "All Sections"
DEFAULT_CATEGORY_FILTER <- "course_satisfaction"

# =============================================================================
# Statistics Display Settings
# =============================================================================

STATISTICS_DISPLAY_N <- TRUE
STATISTICS_DISPLAY_MEAN <- TRUE
STATISTICS_DISPLAY_MEDIAN <- TRUE
STATISTICS_DISPLAY_MODE <- TRUE
STATISTICS_DISPLAY_SD <- TRUE
STATISTICS_DISPLAY_SE <- TRUE
STATISTICS_DISPLAY_MIN <- TRUE
STATISTICS_DISPLAY_MAX <- TRUE
STATISTICS_DISPLAY_Q1 <- TRUE
STATISTICS_DISPLAY_Q3 <- TRUE
STATISTICS_DISPLAY_MISSING <- TRUE

# =============================================================================
# Plot Styling Settings
# =============================================================================

PLOT_THEME <- theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

PLOT_GIRAFE_OPTIONS <- list(
  width = "100%",
  height = "100%",
  zoom_min = 0.5,
  zoom_max = 3,
  zoom_ondblclick = FALSE
)

# =============================================================================
# Error Handling Settings
# =============================================================================

ERROR_TOAST_DURATION <- 5  # seconds
ERROR_TOAST_TYPE <- "error"
WARNING_TOAST_DURATION <- 3  # seconds
WARNING_TOAST_TYPE <- "warning"

# =============================================================================
# Logging Settings
# =============================================================================

LOG_LEVEL <- "WARNING"  # Options: "DEBUG", "INFO", "WARNING", "ERROR", "OFF"
LOG_TO_CONSOLE <- TRUE
LOG_TO_FILE <- FALSE
LOG_FILE_PATH <- "logs/app.log"

# =============================================================================
# Performance Settings
# =============================================================================

DATA_CACHE_ENABLED <- TRUE
PLOT_CACHE_ENABLED <- TRUE
LAZY_LOAD_INSIGHTS <- TRUE

# =============================================================================
# Accessibility Settings
# =============================================================================

ENABLE_SCREEN_READER <- TRUE
ENABLE_FOCUS_INDICATORS <- TRUE
ENABLE_HIGH_CONTRAST_MODE <- FALSE

# =============================================================================
# Export Settings
# =============================================================================

EXPORT_FORMAT <- "csv"
EXPORT_ENCODING <- "UTF-8"
EXPORT_SEPARATOR <- ","
EXPORT_QUOTE <- "\""
EXPORT_DECIMAL <- "."
EXPORT_THOUSANDS <- ","
