# R/global.R
# Global configuration and constants for the CPSC Experience Survey Explorer

# =============================================================================
# Library Imports
# =============================================================================

library(shiny)
library(DT)
library(tidyverse)
library(ggiraph)

# =============================================================================
# Source Configuration Files
# =============================================================================

source("R/app_config.R")

# =============================================================================
# Global Constants - Column Mappings
# =============================================================================

# Core identifier columns
COL_TIMESTAMP <- "Timestamp"
COL_SECTION <- "What section are you in?"
COL_EXPERIENCE <- "Prior to taking this course, what was your programming experience?"
COL_LEARNING_PREF <- "Do you prefer in-person or online learning?"
COL_LEARNING_PREF_REASON <- "Why is this your preferred way of learning?"

# Course satisfaction Likert columns (columns 8-13)
COL_CONTENT_RELEVANT <- "How much do you agree with the statement? [The content is relevant and up-to-date]"
COL_EXCITED_CONTENT <- "How much do you agree with the statement? [I am excited about the content and material that I'm learning]"
COL_SATISFIED_FEEDBACK <- "How much do you agree with the statement? [I'm satisfied with the level of feedback I receive]"
COL_APPLY_LEARNING <- "How much do you agree with the statement? [I feel like I could take what I'm learning and apply it in a new scenario]"
COL_EASY_ASK_HELP <- "How much do you agree with the statement? [It's easy to ask for help]"
COL_MEETING_GOALS <- "How much do you agree with the statement? [I feel like I am meeting the goals of learning Python in this course]"

# Learning methods Likert columns (columns 14-23)
COL_PRE_WRITTEN_CODE <- "How much do the following elements contribute to your learning? [Explanations of pre-written code]"
COL_STUDYING_MIDTERMS <- "How much do the following elements contribute to your learning? [Studying for midterms]"
COL_TOPHAT_QUIZZES <- "How much do the following elements contribute to your learning? [TopHat Quizzes]"
COL_PRESENTATION_SLIDES <- "How much do the following elements contribute to your learning? [Presentation slides]"
COL_HANDOUTS_NOTES <- "How much do the following elements contribute to your learning? [Post-class handouts and notes]"
COL_CODING_OWN <- "How much do the following elements contribute to your learning? [Coding on my own]"
COL_LIVE_CODING <- "How much do the following elements contribute to your learning? [Live coding by the professor]"
COL_LABS <- "How much do the following elements contribute to your learning? [Labs]"
COL_ASK_QUESTIONS <- "How much do the following elements contribute to your learning? [Being able to ask questions of the professor during lecture]"
COL_ASSIGNMENTS <- "How much do the following elements contribute to your learning? [Assignments]"

# Community/belonging Likert columns (columns 27-31)
COL_COMFORTABLE_SPEAKING <- "How much do you agree with the following statements? [I feel comfortable speaking up in class]"
COL_PART_OF_CLASS <- "How much do you agree with the following statements? [I feel like I am a part of this class]"
COL_FRIENDS_IMPORTANT <- "How much do you agree with the following statements? [Making friends within the class is important to me]"
COL_UNIVERSITY_COMMUNITY <- "How much do you agree with the following statements? [I feel like I am a part of the university community]"
COL_EASY_MEET_PEOPLE <- "How much do you agree with the following statements? [It's easy to meet new people within the class]"

# Free-text columns
COL_EXPECTATIONS <- "How is the course meeting your expectations for what you hoped to learn or experience? (Optional)"
COL_PREFERENCE_REASON <- "Why is this your preferred way of learning?"
COL_NOT_PREFERRED_REASON <- "(Optional) If you're not taking this class in your preferred learning method, why?"
COL_IMPROVEMENTS <- "Thinking about what helps you learn the best, if you are going to continue taking programming classes after this one: What do you wish the courses would do more of? And also, what do you wish they would do less of? (Optional)"
COL_FAVORITE_PART <- "What's been your favorite part of the class for you so far, and why? (Optional)"
COL_LEAST_ENJOYABLE <- "What's been the least enjoyable part of class for you so far, and why? (Optional)"
COL_CHALLENGE_MEETING_PEOPLE <- "What's the greatest challenge in meeting new people at university? (Optional)"
COL_INCLUSIVITY <- "Please remark on aspects of the class that make it welcoming and inclusive to you, given your identities and needs, and suggest any aspects that could improve inclusive teaching in this class (Optional)"
COL_STUDENT_INTERACTION <- "What were your expectations/hopes for interacting with the other students? If it isn't meeting your wishes, we'd like to hear more. (Optional)"
COL_PROFESSOR_INTERACTION <- "What were your expectations/hopes for interacting with the professor? If it isn't meeting your wishes, we'd like to hear more. (Optional)"
COL_GENERAL_COMMENTS <- "Any other comments that you would like to share that you feel would make the class more interesting or engaging for you? (Optional)"

# Discord multi-select column
COL_DISCORD <- "About the class Discord (select all that apply)"

# =============================================================================
# Global Constants - Section Identifiers
# =============================================================================

SECTIONS <- c("231 - 1pm", "231 - 11am", "231 - 3pm", 
              "217 - 1pm", "217 - 11am", "217 - 3pm")

# =============================================================================
# Global Constants - Likert Scale Definitions
# =============================================================================

LIKERT_SCALE <- c(
  "1 - Strongly Disagree",
  "2",
  "3",
  "4",
  "5 - Strongly Agree"
)

LIKERT_NUMERIC_VALUES <- c(1, 2, 3, 4, 5)

# =============================================================================
# Global Constants - Question Groupings
# =============================================================================

QUESTION_GROUPS <- list(
  course_satisfaction = c(
    COL_CONTENT_RELEVANT,
    COL_EXCITED_CONTENT,
    COL_SATISFIED_FEEDBACK,
    COL_APPLY_LEARNING,
    COL_EASY_ASK_HELP,
    COL_MEETING_GOALS
  ),
  learning_methods = c(
    COL_PRE_WRITTEN_CODE,
    COL_STUDYING_MIDTERMS,
    COL_TOPHAT_QUIZZES,
    COL_PRESENTATION_SLIDES,
    COL_HANDOUTS_NOTES,
    COL_CODING_OWN,
    COL_LIVE_CODING,
    COL_LABS,
    COL_ASK_QUESTIONS,
    COL_ASSIGNMENTS
  ),
  community_belonging = c(
    COL_COMFORTABLE_SPEAKING,
    COL_PART_OF_CLASS,
    COL_FRIENDS_IMPORTANT,
    COL_UNIVERSITY_COMMUNITY,
    COL_EASY_MEET_PEOPLE
  )
)

# =============================================================================
# Global Constants - Free-Text Question Definitions
# =============================================================================

FREE_TEXT_QUESTIONS <- list(
  q_expectations = COL_EXPECTATIONS,
  q_preference_reason = COL_PREFERENCE_REASON,
  q_not_preferred_reason = COL_NOT_PREFERRED_REASON,
  q_improvements = COL_IMPROVEMENTS,
  q_favorite_part = COL_FAVORITE_PART,
  q_least_enjoyable = COL_LEAST_ENJOYABLE,
  q_challenge_meeting_people = COL_CHALLENGE_MEETING_PEOPLE,
  q_inclusivity = COL_INCLUSIVITY,
  q_student_interaction = COL_STUDENT_INTERACTION,
  q_professor_interaction = COL_PROFESSOR_INTERACTION,
  q_general_comments = COL_GENERAL_COMMENTS
)

# =============================================================================
# Global Constants - Section Definitions
# =============================================================================

SECTION_DEFINITIONS <- list(
  "231 - 1pm" = list(course = 231, time = "1pm"),
  "231 - 11am" = list(course = 231, time = "11am"),
  "231 - 3pm" = list(course = 231, time = "3pm"),
  "217 - 1pm" = list(course = 217, time = "1pm"),
  "217 - 11am" = list(course = 217, time = "11am"),
  "217 - 3pm" = list(course = 217, time = "3pm")
)

# =============================================================================
# Global Constants - Color Palettes
# =============================================================================

COLOR_PALETTE <- list(
  primary = "#2E86AB",
  secondary = "#A23B72",
  success = "#28A745",
  warning = "#FFC107",
  danger = "#DC3545",
  info = "#17A2B8",
  neutral = "#6C757D"
)

LIKERT_COLORS <- c(
  "1 - Strongly Disagree" = "#DC3545",
  "2" = "#F8961E",
  "3" = "#F9C74F",
  "4" = "#90BE6D",
  "5 - Strongly Agree" = "#43AA8B"
)

# =============================================================================
# Global Constants - Data File Path
# =============================================================================

DATA_FILE_PATH <- "survey_data/CPSC Experience Survey.csv"

# =============================================================================
# Global Constants - Default Settings
# =============================================================================

DEFAULT_TABLE_PAGE_SIZE <- 30
DEFAULT_PLOT_WIDTH <- 800
DEFAULT_PLOT_HEIGHT <- 400
DEFAULT_ANIMATION_DURATION <- 300

# =============================================================================
# Source Utility Functions
# =============================================================================

source("R/utils.R")

# =============================================================================
# Source Data Functions
# =============================================================================

source("R/data_loader.R")
source("R/data_transformer.R")

# =============================================================================
# Global Reactive Values (initialized in app.R)
# =============================================================================

# These will be initialized in app.R with reactive values
# survey_data_raw <- reactiveVal(NULL)
# survey_data_clean <- reactiveVal(NULL)
# selected_section <- reactiveVal(NULL)
