# CPSC Experience Survey Explorer - Global Configuration
# This file is sourced at application startup to load libraries,
# configuration, and data processing functions.
#
# @author Course Instructor
# @version 2.0.0

# ==============================================================================
# LIBRARY IMPORTS
# ==============================================================================

# Shiny web application framework
library(shiny)

# bslib for Bootstrap theming
library(bslib)

# DataTables for interactive tables
library(DT)

# Tidyverse packages for data manipulation
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

# ggplot2 for visualizations
library(ggplot2)

# ggiraph for interactive ggplot2 graphics
library(ggiraph)

# reshape2 for data reshaping
library(reshape2)

# Additional utilities
library(tools)

# ==============================================================================
# APP CONFIGURATION CONSTANTS
# ==============================================================================

#' Application metadata
#' @export
APP_CONFIG <- list(
  title = "CPSC Experience Survey Explorer",
  version = "2.0.0",
  data_file = "survey_data/CPSC Experience Survey.csv",
  author = "Course Instructor",
  description = "Interactive survey data explorer for CPSC course experience feedback"
)

#' Likert scale configuration
#' @export
LIKERT_CONFIG <- list(
  min_value = 1,
  max_value = 5,
  labels = c(
    "1" = "Strongly Disagree",
    "2" = "Disagree",
    "3" = "Neutral",
    "4" = "Agree",
    "5" = "Strongly Agree"
  ),
  colors = c(
    "1" = "#d73027",
    "2" = "#fc8d59",
    "3" = "#fee08b",
    "4" = "#d9ef8b",
    "5" = "#1a9850"
  )
)

#' Question category definitions
#' @export
QUESTION_CATEGORIES <- list(
  course_satisfaction = list(
    name = "Course Satisfaction",
    questions = c(
      "The content is relevant and up-to-date",
      "I am excited about the content and material that I'm learning",
      "I'm satisfied with the level of feedback I receive",
      "I feel like I could take what I'm learning and apply it in a new scenario",
      "It's easy to ask for help",
      "I feel like I am meeting the goals of learning Python in this course"
    )
  ),
  learning_methods = list(
    name = "Learning Methods",
    questions = c(
      "Explanations of pre-written code",
      "Studying for midterms",
      "TopHat Quizzes",
      "Presentation slides",
      "Post-class handouts and notes",
      "Coding on my own",
      "Live coding by the professor",
      "Labs",
      "Asking questions of the professor during lecture",
      "Assignments"
    )
  ),
  community_belonging = list(
    name = "Community & Belonging",
    questions = c(
      "I feel comfortable speaking up in class",
      "I feel like I am a part of this class",
      "Making friends within the class is important to me",
      "I feel like I am a part of the university community",
      "It's easy to meet new people within the class"
    )
  )
)

#' Free-text question definitions
#' @export
FREE_TEXT_QUESTIONS <- list(
  expectations = "How is the course meeting your expectations for what you hoped to learn or experience? (Optional)",
  preferred_method_reason = "Why is this your preferred way of learning?",
  not_preferred_reason = "(Optional) If you're not taking this class in your preferred learning method, why?",
  course_improvements = "Thinking about what helps you learn the best, if you are going to continue taking programming classes after this one: What do you wish the courses would do more of? And also, what do you wish they would do less of? (Optional)",
  favorite_part = "What's been your favorite part of the class for you so far, and why? (Optional)",
  least_enjoyable = "What's been the least enjoyable part of class for you so far, and why? (Optional)",
  meeting_challenge = "What's the greatest challenge in meeting new people at university? (Optional)",
  inclusivity = "Please remark on aspects of the class that make it welcoming and inclusive to you, given your identities and needs, and suggest any aspects that could improve inclusive teaching in this class (Optional)",
  student_interaction = "What were your expectations/hopes for interacting with the other students? If it isn't meeting your wishes, we'd like to hear more. (Optional)",
  professor_interaction = "What were your expectations/hopes for interacting with the professor? If it isn't meeting your wishes, we'd like to hear more. (Optional)",
  other_comments = "Any other comments that you would like to share that you feel would make the class more interesting or engaging for you? (Optional)"
)

#' Section identifiers
#' @export
SECTIONS <- c(
  "231 - 11am",
  "231 - 1pm",
  "231 - 3pm",
  "217 - 11am",
  "217 - 1pm",
  "217 - 3pm"
)

# ==============================================================================
# DATA LOADING AND PREPROCESSING
# ==============================================================================

#' Load survey data from CSV file
#'
#' Reads the survey data CSV and performs initial preprocessing.
#'
#' @param file_path Path to the CSV file (relative to app directory)
#' @return Data frame with survey responses
#' @export
load_survey_data <- function(file_path = APP_CONFIG$data_file) {
  if (!file.exists(file_path)) {
    stop(
      "Data file not found: ", file_path, "\n",
      "Please ensure the survey data file exists and the path is correct.\n",
      "Expected path: ", normalizePath(file_path, mustWork = FALSE)
    )
  }

  data <- utils::read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

  # Clean column names
  colnames(data) <- tools::toTitleCase(tolower(colnames(data)))
  colnames(data) <- base::trimws(colnames(data))

  # Add row ID for tracking
  data$response_id <- seq_len(nrow(data))

  return(data)
}

#' Process all Likert scale columns in survey data
#'
#' Converts Likert response columns to numeric format
#' Uses parse_likert from R/utils_data_processing.R
#'
#' @param data Survey data frame
#' @param likert_cols Vector of column names containing Likert responses
#' @return Data frame with processed Likert columns
#' @export
process_likert_columns <- function(data, likert_cols) {
  for (col in likert_cols) {
    if (col %in% colnames(data)) {
      data[[col]] <- sapply(data[[col]], parse_likert)
    }
  }
  return(data)
}

#' Parse section identifier into components
#'
#' Extracts course number and time slot from section string
#'
#' @param section Section string (e.g., "231 - 1pm")
#' @return List with course_number and time_slot
#' @export
parse_section <- function(section) {
  if (is.na(section) || section == "") {
    return(list(course_number = NA, time_slot = NA))
  }

  parts <- stringr::str_split_fixed(section, " - ", n = 2)

  return(list(
    course_number = base::trimws(parts[1]),
    time_slot = base::trimws(parts[2])
  ))
}

#' Parse Discord multi-select responses
#'
#' Splits semicolon-separated Discord responses into separate columns
#'
#' @param data Survey data frame
#' @return Data frame with added Discord indicator columns
#' @export
process_discord_responses <- function(data) {
  discord_col <- "About the class Discord (select all that apply)"

  if (!discord_col %in% colnames(data)) {
    return(data)
  }

  # Initialize indicator columns
  data$discord_joined <- FALSE
  data$discord_active <- FALSE
  data$discord_useful <- FALSE

  # Parse responses
  for (i in seq_len(nrow(data))) {
    response <- data[[discord_col]][i]
    if (!is.na(response) && response != "") {
      responses <- stringr::str_split(response, ";")[[1]]
      data$discord_joined[i] <- any(stringr::str_detect(responses, "joined"))
      data$discord_active[i] <- any(stringr::str_detect(responses, "active"))
      data$discord_useful[i] <- any(stringr::str_detect(responses, "useful"))
    }
  }

  return(data)
}

#' Get all Likert scale column names from survey data
#'
#' Identifies columns containing Likert scale responses
#'
#' @param data Survey data frame
#' @return Vector of column names
#' @export
get_likert_columns <- function(data) {
  likert_patterns <- c(
    "How much do you agree with the statement?",
    "How much do the following elements contribute to your learning?",
    "How much do you agree with the following statements?"
  )

  cols <- colnames(data)
  likert_cols <- character(0)

  for (pattern in likert_patterns) {
    matches <- stringr::str_detect(cols, stringr::regex(pattern, ignore_case = TRUE))
    likert_cols <- c(likert_cols, cols[matches])
  }

  return(likert_cols)
}

#' Filter data by section
#'
#' Filters survey data to a specific section
#'
#' @param data Survey data frame
#' @param section Section identifier (e.g., "231 - 1pm") or NULL for all
#' @return Filtered data frame
#' @export
filter_by_section <- function(data, section = NULL) {
  if (is.null(section) || is.na(section) || section == "") {
    return(data)
  }

  section_col <- "What section are you in?"
  if (section_col %in% colnames(data)) {
    return(dplyr::filter(data, .data[[section_col]] == section))
  }

  return(data)
}

#' Calculate summary statistics for a Likert question
#'
#' Computes descriptive statistics for a single Likert-scale question
#'
#' @param data Survey data frame
#' @param question_col Column name of the question
#' @return List of summary statistics
#' @export
calculate_likert_summary <- function(data, question_col) {
  values <- data[[question_col]]
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(list(
      n = 0,
      mean = NA,
      median = NA,
      mode = NA,
      sd = NA,
      se = NA,
      min = NA,
      max = NA,
      q1 = NA,
      q3 = NA,
      missing = nrow(data)
    ))
  }

  n <- length(values)
  mean_val <- mean(values)
  median_val <- median(values)
  mode_val <- as.integer(names(which.max(table(values))))
  sd_val <- sd(values)
  se_val <- sd_val / sqrt(n)
  min_val <- min(values)
  max_val <- max(values)
  q1_val <- quantile(values, 0.25)
  q3_val <- quantile(values, 0.75)
  missing_count <- nrow(data) - n

  return(list(
    n = n,
    mean = mean_val,
    median = median_val,
    mode = mode_val,
    sd = sd_val,
    se = se_val,
    min = min_val,
    max = max_val,
    q1 = q1_val,
    q3 = q3_val,
    missing = missing_count
  ))
}

# ==============================================================================
# THEME SETUP
# ==============================================================================

#' Get application theme
#'
#' Returns a bslib theme for the Shiny application
#'
#' @return bs_theme object
#' @export
get_app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#3498db",
    success = "#27ae60",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = bslib::font_google("Lato"),
    heading_font = bslib::font_google("Montserrat")
  )
}

#' Default ggplot2 theme for survey visualizations
#'
#' @return ggplot2 theme object
#' @export
get_viz_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        size = 12,
        hjust = 0.5
      ),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ==============================================================================
# INITIALIZE APPLICATION DATA
# ==============================================================================

# Load and preprocess data at startup
tryCatch({
  survey_data <- load_survey_data()

  # Identify Likert columns
  likert_columns <- get_likert_columns(survey_data)

  # Process Likert columns to numeric
  survey_data <- process_likert_columns(survey_data, likert_columns)

  # Process Discord responses
  survey_data <- process_discord_responses(survey_data)

  # Make data available globally
  SURVEY_DATA <<- survey_data
  LIKERT_COLUMNS <<- likert_columns

  message("Survey data loaded successfully: ", nrow(survey_data), " responses")
}, error = function(e) {
  message("ERROR: Failed to load survey data - ", e$message)
  message("The application will start with empty data.")
  SURVEY_DATA <<- data.frame()
  LIKERT_COLUMNS <<- character(0)
})
