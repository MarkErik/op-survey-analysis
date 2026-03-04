R/utils_insights.R
Helper functions for Insights tab statistical analyses

calculate_correlation_matrix <- function(data) {
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

  cor_matrix <- cor(likert_data, use = "pairwise.complete.obs")

  return(cor_matrix)
}

perform_cluster_analysis <- function(data, k = 4) {
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

  likert_scaled <- scale(likert_data)

  set.seed(42)
  kmeans_result <- kmeans(likert_scaled, centers = k, nstart = 10)

  likert_data$cluster <- kmeans_result$cluster

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

  cluster_profiles$ClusterName <- sapply(cluster_profiles$cluster, function(c) {
    if (c == 1) return("Highly Engaged")
    if (c == 2) return("Struggling")
    if (c == 3) return("Satisfied but Quiet")
    if (c == 4) return("Moderately Engaged")
    return("Mixed")
  })

  return(list(clusters = cluster_profiles, profiles = likert_data))
}

calculate_cronbachs_alpha <- function(data, variables) {
  likert_data <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(variables)), ~ extract_likert_value(.)) %>%
    dplyr::filter(!dplyr::any_of(dplyr::all_of(variables)))

  if (nrow(likert_data) < 3) {
    return(NA)
  }

  alpha_result <- psych::alpha(likert_data[, variables])$total$alpha

  return(alpha_result)
}

perform_regression_analysis <- function(data, outcome, predictors) {
  reg_data <- data %>+
    dplyr::select(dplyr::all_of(outcome), dplyr::all_of(predictors)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(outcome, predictors))), ~ extract_likert_value(.)) %>%
    dplyr::filter(!dplyr::any_of(dplyr::all_of(c(outcome, predictors))))

  if (nrow(reg_data) == 0) {
    return(tibble::tibble())
  }

  model <- lm(dplyr::all_of(outcome) ~ ., data = reg_data[, c(outcome, predictors)])

  coef_summary <- summary(model)$coefficients

  predictors_df <- data.frame(
    Predictor = predictors,
    Coefficient = round(coef_summary[, 1], 4),
    StdError = round(coef_summary[, 2], 4),
    tValue = round(coef_summary[, 3], 4),
    PValue = round(coef_summary[, 4], 4),
    stringsAsFactors = FALSE
  )

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

calculate_cohens_d <- function(group1, group2) {
  if (is.null(group1) || length(group1) == 0) {
    return(list(d = NA, interpretation = "Insufficient data"))
  }

  if (is.null(group2) || length(group2) == 0) {
    return(list(d = NA, interpretation = "Insufficient data"))
  }

  group1 <- group1[!is.na(group1)]
  group2 <- group2[!is.na(group2)]

  if (length(group1) == 0 || length(group2) == 0) {
    return(list(d = NA, interpretation = "Insufficient data"))
  }

  n1 <- length(group1)
  n2 <- length(group2)
  pooled_sd <- sqrt(((n1 - 1) * var(group1) + (n2 - 1) * var(group2)) / (n1 + n2 - 2))

  d <- (mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)) / pooled_sd

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

calculate_section_comparison <- function(data, question_col) {
  comp_data <- data %>%
    dplyr::select(dplyr::all_of(COL_SECTION), dplyr::all_of(question_col)) %>%
    dplyr::mutate(value = extract_likert_value(dplyr::all_of(question_col))) %>%
    dplyr::filter(!is.na(value))

  if (nrow(comp_data) == 0) {
    return(tibble::tibble())
  }

  section_stats <- comp_data %>+
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

test_interaction <- function(data, outcome, var1, var2) {
  int_data <- data %>%
    dplyr::select(dplyr::all_of(outcome), dplyr::all_of(var1), dplyr::all_of(var2)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(outcome, var1, var2))), ~ extract_likert_value(.)) %>%
    dplyr::filter(!dplyr::any_of(dplyr::all_of(c(outcome, var1, var2))))

  if (nrow(int_data) == 0) {
    return(tibble::tibble())
  }

  model <- lm(dplyr::all_of(outcome) ~ dplyr::all_of(var1) * dplyr::all_of(var2), data = int_data)
  summary_result <- summary(model)$coefficients

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
