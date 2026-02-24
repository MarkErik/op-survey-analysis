# R/utils_insights.R
# Helper functions for Insights tab statistical analyses

# =============================================================================
# Correlation Analysis Helpers
# =============================================================================

#' Calculate correlation matrix between Likert questions
#'
#' Computes Pearson correlations between all Likert-scale questions in the survey.
#'
#' @param data Tibble containing processed survey data
#' @return Numeric matrix of correlation coefficients
#' @examples
#' calculate_correlation_matrix(processed_data)
calculate_correlation_matrix <- function(data) {
  # Extract Likert values for all question groups
  likert_data <- data %>%
    dplyr::select(
      dplyr::all_of(COL_CONTENT_RELEVANT),
      dplyr::all_of(COL_EXCITED_CONTENT),
      dplyr::all_of(COL_SATISFIED_FEEDBACK),
      dplyr::all_of(COL_APPLY_LEARNING),
      dplyr::all_of(COL_EASY_ASK_HELP),
      dplyr::all_of(COL_MEETING_GOALS),
      dplyr::all_of(COL_PRE_WRITTEN_CODE),
      dplyr::all_of(COL_STUDYING_MIDTERMS),
      dplyr::all_of(COL_TOPHAT_QUIZZES),
      dplyr::all_of(COL_PRESENTATION_SLIDES),
      dplyr::all_of(COL_HANDOUTS_NOTES),
      dplyr::all_of(COL_CODING_OWN),
      dplyr::all_of(COL_LIVE_CODING),
      dplyr::all_of(COL_LABS),
      dplyr::all_of(COL_ASK_QUESTIONS),
      dplyr::all_of(COL_ASSIGNMENTS),
      dplyr::all_of(COL_COMFORTABLE_SPEAKING),
      dplyr::all_of(COL_PART_OF_CLASS),
      dplyr::all_of(COL_FRIENDS_IMPORTANT),
      dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
      dplyr::all_of(COL_EASY_MEET_PEOPLE)
    ) %>%
    dplyr::mutate(
      content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
      excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
      satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
      apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
      easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
      meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
      pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
      studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
      tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
      presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
      handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
      coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
      live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
      labs = extract_likert_value(dplyr::all_of(COL_LABS)),
      ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
      assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
      comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
      part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
      friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
      university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
      easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
    ) %>%
    dplyr::filter(!dplyr::any_of(dplyr::c(
      content_relevant, excited_content, satisfied_feedback,
      apply_learning, easy_ask_help, meeting_goals,
      pre_written_code, studying_midterms, tophat_quizzes,
      presentation_slides, handouts_notes, coding_own,
      live_coding, labs, ask_questions, assignments,
      comfortable_speaking, part_of_class, friends_important,
      university_community, easy_meet_people
    )))

  if (nrow(likert_data) == 0) {
    return(NULL)
  }

  # Calculate correlation matrix
  cor_matrix <- cor(likert_data, use = "pairwise.complete.obs")

  return(cor_matrix)
}

# =============================================================================
# Clustering Analysis Helpers
# =============================================================================

#' Perform K-means clustering on student response patterns
#'
#' Groups students based on their Likert-scale responses using K-means clustering.
#'
#' @param data Tibble containing processed survey data
#' @param k Integer number of clusters to create (default: 4)
#' @return List containing cluster assignments and cluster profiles
#' @examples
#' perform_cluster_analysis(processed_data, k = 4)
perform_cluster_analysis <- function(data, k = 4) {
  # Extract Likert values for all question groups
  likert_data <- data %>%
    dplyr::select(
      dplyr::all_of(COL_CONTENT_RELEVANT),
      dplyr::all_of(COL_EXCITED_CONTENT),
      dplyr::all_of(COL_SATISFIED_FEEDBACK),
      dplyr::all_of(COL_APPLY_LEARNING),
      dplyr::all_of(COL_EASY_ASK_HELP),
      dplyr::all_of(COL_MEETING_GOALS),
      dplyr::all_of(COL_PRE_WRITTEN_CODE),
      dplyr::all_of(COL_STUDYING_MIDTERMS),
      dplyr::all_of(COL_TOPHAT_QUIZZES),
      dplyr::all_of(COL_PRESENTATION_SLIDES),
      dplyr::all_of(COL_HANDOUTS_NOTES),
      dplyr::all_of(COL_CODING_OWN),
      dplyr::all_of(COL_LIVE_CODING),
      dplyr::all_of(COL_LABS),
      dplyr::all_of(COL_ASK_QUESTIONS),
      dplyr::all_of(COL_ASSIGNMENTS),
      dplyr::all_of(COL_COMFORTABLE_SPEAKING),
      dplyr::all_of(COL_PART_OF_CLASS),
      dplyr::all_of(COL_FRIENDS_IMPORTANT),
      dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
      dplyr::all_of(COL_EASY_MEET_PEOPLE)
    ) %>%
    dplyr::mutate(
      content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
      excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
      satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
      apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
      easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
      meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
      pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
      studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
      tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
      presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
      handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
      coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
      live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
      labs = extract_likert_value(dplyr::all_of(COL_LABS)),
      ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
      assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
      comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
      part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
      friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
      university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
      easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
    ) %>%
    dplyr::filter(!dplyr::any_of(dplyr::c(
      content_relevant, excited_content, satisfied_feedback,
      apply_learning, easy_ask_help, meeting_goals,
      pre_written_code, studying_midterms, tophat_quizzes,
      presentation_slides, handouts_notes, coding_own,
      live_coding, labs, ask_questions, assignments,
      comfortable_speaking, part_of_class, friends_important,
      university_community, easy_meet_people
    )))

  if (nrow(likert_data) == 0) {
    return(list(clusters = tibble::tibble(), profiles = tibble::tibble()))
  }

  # Standardize data
  likert_scaled <- scale(likert_data)

  # Perform K-means clustering
  set.seed(42)
  kmeans_result <- kmeans(likert_scaled, centers = k, nstart = 10)

  # Add cluster labels to data
  likert_data$cluster <- kmeans_result$cluster

  # Calculate cluster profiles (mean scores for each question)
  cluster_profiles <- likert_data %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      content_relevant = mean(content_relevant, na.rm = TRUE),
      excited_content = mean(excited_content, na.rm = TRUE),
      satisfied_feedback = mean(satisfied_feedback, na.rm = TRUE),
      apply_learning = mean(apply_learning, na.rm = TRUE),
      easy_ask_help = mean(easy_ask_help, na.rm = TRUE),
      meeting_goals = mean(meeting_goals, na.rm = TRUE),
      pre_written_code = mean(pre_written_code, na.rm = TRUE),
      studying_midterms = mean(studying_midterms, na.rm = TRUE),
      tophat_quizzes = mean(tophat_quizzes, na.rm = TRUE),
      presentation_slides = mean(presentation_slides, na.rm = TRUE),
      handouts_notes = mean(handouts_notes, na.rm = TRUE),
      coding_own = mean(coding_own, na.rm = TRUE),
      live_coding = mean(live_coding, na.rm = TRUE),
      labs = mean(labs, na.rm = TRUE),
      ask_questions = mean(ask_questions, na.rm = TRUE),
      assignments = mean(assignments, na.rm = TRUE),
      comfortable_speaking = mean(comfortable_speaking, na.rm = TRUE),
      part_of_class = mean(part_of_class, na.rm = TRUE),
      friends_important = mean(friends_important, na.rm = TRUE),
      university_community = mean(university_community, na.rm = TRUE),
      easy_meet_people = mean(easy_meet_people, na.rm = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(meeting_goals))

  # Add cluster names based on profiles
  cluster_profiles$ClusterName <- sapply(cluster_profiles$cluster, function(c) {
    if (c == 1) return("Highly Engaged")
    if (c == 2) return("Struggling")
    if (c == 3) return("Satisfied but Quiet")
    if (c == 4) return("Moderately Engaged")
    return("Mixed")
  })

  return(list(clusters = cluster_profiles, profiles = likert_data))
}

# =============================================================================
# Cronbach's Alpha Helpers
# =============================================================================

#' Calculate Cronbach's alpha for a set of variables
#'
#' Measures internal consistency reliability of a scale.
#'
#' @param data Tibble containing processed survey data
#' @param variables Character vector of variable names to include in the scale
#' @return Cronbach's alpha value
#' @examples
#' calculate_cronbachs_alpha(processed_data, c("content_relevant", "excited_content"))
calculate_cronbachs_alpha <- function(data, variables) {
  # Extract Likert values for specified variables
  likert_data <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(variables)), ~ extract_likert_value(.)) %>%
    dplyr::filter(!dplyr::any_of(dplyr::all_of(variables)))

  if (nrow(likert_data) < 3) {
    return(NA)
  }

  # Calculate Cronbach's alpha using psych package
  alpha_result <- psych::alpha(likert_data[, variables])$total$alpha

  return(alpha_result)
}

# =============================================================================
# Regression Analysis Helpers
# =============================================================================

#' Perform linear regression analysis
#'
#' Identifies predictors of a given outcome variable.
#'
#' @param data Tibble containing processed survey data
#' @param outcome Character name of the outcome variable
#' @param predictors Character vector of predictor variable names
#' @return Data frame with regression results
#' @examples
#' perform_regression_analysis(processed_data, "meeting_goals", c("content_relevant", "excited_content"))
perform_regression_analysis <- function(data, outcome, predictors) {
  # Extract Likert values
  reg_data <- data %>%
    dplyr::select(dplyr::all_of(outcome), dplyr::all_of(predictors)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(outcome, predictors))), ~ extract_likert_value(.)) %>%
    dplyr::filter(!dplyr::any_of(dplyr::all_of(c(outcome, predictors))))

  if (nrow(reg_data) == 0) {
    return(tibble::tibble())
  }

  # Fit linear regression
  model <- lm(dplyr::all_of(outcome) ~ ., data = reg_data[, c(outcome, predictors)])

  # Extract coefficients and p-values
  coef_summary <- summary(model)$coefficients

  # Create predictors table
  predictors_df <- data.frame(
    Predictor = predictors,
    Coefficient = round(coef_summary[, 1], 4),
    StdError = round(coef_summary[, 2], 4),
    tValue = round(coef_summary[, 3], 4),
    PValue = round(coef_summary[, 4], 4),
    stringsAsFactors = FALSE
  )

  # Add interpretation
  predictors_df$Interpretation <- sapply(predictors_df$Coefficient, function(c) {
    if (is.na(c)) return("N/A")
    if (c > 0) return("Positive effect")
    if (c < 0) return("Negative effect")
    return("No effect")
  })

  # Sort by absolute coefficient
  predictors_df <- predictors_df %>%
    dplyr::arrange(dplyr::desc(abs(Coefficient)))

  return(predictors_df)
}

# =============================================================================
# Effect Size Helpers
# =============================================================================

#' Calculate Cohen's d effect size between two groups
#'
#' @param group1 Numeric vector for group 1
#' @param group2 Numeric vector for group 2
#' @return List with effect size and interpretation
#' @examples
#' calculate_cohens_d(c(1,2,3), c(4,5,6))
calculate_cohens_d <- function(group1, group2) {
  # Handle NULL or empty inputs
  if (is.null(group1) || length(group1) == 0) {
    return(list(d = NA, interpretation = "Insufficient data"))
  }

  if (is.null(group2) || length(group2) == 0) {
    return(list(d = NA, interpretation = "Insufficient data"))
  }

  # Remove NA values
  group1 <- group1[!is.na(group1)]
  group2 <- group2[!is.na(group2)]

  if (length(group1) == 0 || length(group2) == 0) {
    return(list(d = NA, interpretation = "Insufficient data"))
  }

  # Calculate pooled standard deviation
  n1 <- length(group1)
  n2 <- length(group2)
  pooled_sd <- sqrt(((n1 - 1) * var(group1) + (n2 - 1) * var(group2)) / (n1 + n2 - 2))

  # Calculate Cohen's d
  d <- (mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)) / pooled_sd

  # Determine interpretation
  interpretation <- switch(
    as.character(abs(d)),
    "0" = "No effect",
    "0.2" = "Small effect",
    "0.5" = "Medium effect",
    "0.8" = "Large effect",
    ifelse(abs(d) < 0.2, "Small effect",
           ifelse(abs(d) < 0.5, "Medium effect",
                  ifelse(abs(d) < 0.8, "Large effect", "Very large effect")))
  )

  return(list(d = d, interpretation = interpretation))
}

# =============================================================================
# Section Comparison Helpers
# =============================================================================

#' Calculate section comparison statistics
#'
#' @param data Tibble containing processed survey data
#' @param question_col Character name of the question column
#' @return Data frame with section statistics
#' @examples
#' calculate_section_comparison(processed_data, "meeting_goals")
calculate_section_comparison <- function(data, question_col) {
  # Extract Likert values and section for each question
  comp_data <- data %>%
    dplyr::select(dplyr::all_of(COL_SECTION), dplyr::all_of(question_col)) %>%
    dplyr::mutate(value = extract_likert_value(dplyr::all_of(question_col))) %>%
    dplyr::filter(!is.na(value))

  if (nrow(comp_data) == 0) {
    return(tibble::tibble())
  }

  # Calculate mean scores by section
  section_stats <- comp_data %>%
    dplyr::group_by(dplyr::all_of(COL_SECTION)) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(mean))

  return(section_stats)
}

# =============================================================================
# Interaction Analysis Helpers
# =============================================================================

#' Test interaction effects between variables
#'
#' @param data Tibble containing processed survey data
#' @param outcome Character name of the outcome variable
#' @param var1 Character name of first predictor variable
#' @param var2 Character name of second predictor variable
#' @return Data frame with interaction test results
#' @examples
#' test_interaction(processed_data, "meeting_goals", "content_relevant", "excited_content")
test_interaction <- function(data, outcome, var1, var2) {
  # Extract Likert values
  int_data <- data %>%
    dplyr::select(dplyr::all_of(outcome), dplyr::all_of(var1), dplyr::all_of(var2)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(outcome, var1, var2))), ~ extract_likert_value(.)) %>%
    dplyr::filter(!dplyr::any_of(dplyr::all_of(c(outcome, var1, var2))))

  if (nrow(int_data) == 0) {
    return(tibble::tibble())
  }

  # Fit linear regression with interaction term
  model <- lm(dplyr::all_of(outcome) ~ dplyr::all_of(var1) * dplyr::all_of(var2), data = int_data)
  summary_result <- summary(model)$coefficients

  # Extract interaction term results
  if (nrow(summary_result) >= 3) {
    interaction_row <- summary_result[3, ]
    return(data.frame(
      Interaction = paste(var1, "×", var2),
      FValue = round(interaction_row[4], 4),
      PValue = round(interaction_row[5], 4),
      Interpretation = ifelse(interaction_row[5] < 0.05, "Significant",
                               ifelse(interaction_row[5] < 0.1, "Marginally significant", "Not significant")),
      stringsAsFactors = FALSE
    ))
  }

  return(tibble::tibble())
}
