# Advanced Exploratory Data Analysis (EDA) Script for Key Insights
# This script performs statistical analysis to identify non-obvious insights
# that could inform instructional design decisions.

# Required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)
library(stringr)
library(corrplot)
library(psych)
library(car)
library(ggpubr)
library(cluster)
library(factoextra)

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================

load_data <- function() {
  tryCatch({
    df <- read.csv("survey_data/exported_data.csv", stringsAsFactors = FALSE)
    # Clean column names
    names(df) <- tolower(names(df))
    # Remove any leading/trailing whitespace from character columns
    df <- df %>% mutate(across(where(is.character), str_trim))
    message("Data loaded successfully: ", nrow(df), " rows, ", ncol(df), " columns")
    return(df)
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

# Convert text responses to numeric scores (1-5)
convert_to_numeric <- function(responses, scale) {
  # Remove all non-numeric characters to extract the score
  # Handles formats like "5 - Strongly Agree", "4", "3", etc.
  numeric_values <- sapply(responses, function(r) {
    if (is.na(r) || r == "") {
      return(NA)
    }
    
    # Extract numeric value by removing all non-numeric characters
    numeric_value <- as.numeric(gsub("[^0-9]", "", r))
    
    return(numeric_value)
  })
  
  return(as.numeric(numeric_values))
}

# Define Likert scale questions and their response mappings
likert_questions <- list(
  course = list(
    excited = list(
      column = "x.course..i.am.excited.about.the.content.and.material.that.i.m.learning",
      label = "Excited about content",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    relevant = list(
      column = "x.course..the.content.is.relevant.and.up.to.date",
      label = "Content is relevant",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    meeting_goals = list(
      column = "x.course..i.feel.like.i.am.meeting.the.goals.of.learning.python.in.this.course",
      label = "Meeting learning goals",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    apply_scenario = list(
      column = "x.course..i.feel.like.i.could.take.what.i.m.learning.and.apply.it.in.a.new.scenario",
      label = "Can apply to new scenarios",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    feedback = list(
      column = "x.course..i.m.satisfied.with.the.level.of.feedback.i.receive",
      label = "Satisfied with feedback",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    ask_help = list(
      column = "x.course..it.s.easy.to.ask.for.help",
      label = "Easy to ask for help",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    )
  ),
  learning = list(
    pre_written_code = list(
      column = "x.learning..explanations.of.pre.written.code",
      label = "Pre-written code explanations",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    live_coding = list(
      column = "x.learning..live.coding.by.the.professor",
      label = "Live coding",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    slides = list(
      column = "x.learning..presentation.slides",
      label = "Presentation slides",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    handouts = list(
      column = "x.learning..post.class.handouts.and.notes",
      label = "Handouts and notes",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    tophat_quizzes = list(
      column = "x.learning..tophat.quizzes",
      label = "TopHat quizzes",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    assignments = list(
      column = "x.learning..assignments",
      label = "Assignments",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    labs = list(
      column = "x.learning..labs",
      label = "Labs",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    ask_questions = list(
      column = "x.learning..being.able.to.ask.questions.of.the.professor.during.lecture",
      label = "Asking questions",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    studying_midterms = list(
      column = "x.learning..studying.for.midterms",
      label = "Studying for midterms",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    coding_own = list(
      column = "x.learning..coding.on.my.own",
      label = "Coding on my own",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    )
  ),
  community = list(
    friends_important = list(
      column = "x.community..making.friends.within.the.class.is.important.to.me",
      label = "Friends important",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    easy_meet = list(
      column = "x.community..it.s.easy.to.meet.new.people.within.the.class",
      label = "Easy to meet people",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    part_of_class = list(
      column = "x.community..i.feel.like.i.am.a.part.of.this.class",
      label = "Feel part of class",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    comfortable_speaking = list(
      column = "x.community..i.feel.comfortable.speaking.up.in.class",
      label = "Comfortable speaking",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    part_of_university = list(
      column = "x.community..i.feel.like.i.am.a.part.of.the.university.community",
      label = "Feel part of university",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    )
  )
)

# =============================================================================
# DATA PREPARATION FOR ANALYSIS
# =============================================================================

prepare_analysis_data <- function(df) {
  # Create a data frame with numeric values for all Likert questions
  numeric_df <- data.frame(
    row_id = 1:nrow(df),
    section = df$section
  )
  
  # Convert all Likert questions to numeric
  for (category in names(likert_questions)) {
    for (question_name in names(likert_questions[[category]])) {
      question_info <- likert_questions[[category]][[question_name]]
      
      if (question_info$column %in% names(df)) {
        numeric_values <- convert_to_numeric(df[[question_info$column]], question_info$scale)
        numeric_df[[question_name]] <- numeric_values
      }
    }
  }
  
  # Remove rows with too many missing values
  missing_per_row <- rowSums(is.na(numeric_df[, -c(1, 2)]))
  numeric_df <- numeric_df[missing_per_row <= 5, ]
  
  message("Analysis data prepared: ", nrow(numeric_df), " complete cases")
  
  return(numeric_df)
}

# =============================================================================
# INSIGHT 1: CORRELATION ANALYSIS - What predicts learning outcomes?
# =============================================================================

analyze_correlations <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 1: CORRELATION ANALYSIS")
  message("========================================\n")
  
  # Select relevant columns for correlation
  outcome_vars <- c("meeting_goals", "apply_scenario", "excited", "relevant")
  learning_vars <- c("pre_written_code", "live_coding", "slides", "handouts", 
                     "tophat_quizzes", "assignments", "labs", "ask_questions",
                     "studying_midterms", "coding_own")
  community_vars <- c("friends_important", "easy_meet", "part_of_class", 
                      "comfortable_speaking", "part_of_university")
  
  # Calculate correlations with learning outcomes
  correlations <- data.frame()
  
  for (outcome in outcome_vars) {
    if (outcome %in% names(numeric_df)) {
      for (var in c(learning_vars, community_vars)) {
        if (var %in% names(numeric_df)) {
          cor_test <- cor.test(numeric_df[[outcome]], numeric_df[[var]], 
                               use = "complete.obs", method = "pearson")
          
          correlations <- rbind(correlations, data.frame(
            Outcome = outcome,
            Predictor = var,
            Correlation = round(cor_test$estimate, 3),
            P_Value = round(cor_test$p.value, 4),
            Significant = cor_test$p.value < 0.05
          ))
        }
      }
    }
  }
  
  # Sort by absolute correlation
  correlations$Abs_Correlation <- abs(correlations$Correlation)
  correlations <- correlations[order(-correlations$Abs_Correlation), ]
  
  # Print top correlations
  message("Top 15 correlations with learning outcomes:\n")
  print(head(correlations[, c("Outcome", "Predictor", "Correlation", "P_Value", "Significant")], 15), 
        row.names = FALSE)
  
  # Create correlation heatmap
  all_vars <- c(outcome_vars, learning_vars, community_vars)
  available_vars <- all_vars[all_vars %in% names(numeric_df)]
  
  cor_matrix <- cor(numeric_df[, available_vars], use = "complete.obs")
  
  # Create heatmap
  p1 <- corrplot(cor_matrix, method = "color", type = "upper", 
                 tl.col = "black", tl.srt = 45, 
                 title = "Correlation Matrix of Survey Variables",
                 mar = c(0, 0, 2, 0))
  
  return(list(correlations = correlations, cor_matrix = cor_matrix))
}

# =============================================================================
# INSIGHT 2: REGRESSION ANALYSIS - What drives meeting learning goals?
# =============================================================================

analyze_regression <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 2: REGRESSION ANALYSIS")
  message("========================================\n")
  
  # Prepare data for regression
  learning_vars <- c("pre_written_code", "live_coding", "slides", "handouts", 
                     "tophat_quizzes", "assignments", "labs", "ask_questions",
                     "studying_midterms", "coding_own")
  community_vars <- c("friends_important", "easy_meet", "part_of_class", 
                      "comfortable_speaking", "part_of_university")
  
  available_predictors <- c(learning_vars, community_vars)
  available_predictors <- available_predictors[available_predictors %in% names(numeric_df)]
  
  # Multiple regression for meeting_goals
  if ("meeting_goals" %in% names(numeric_df)) {
    formula_str <- paste("meeting_goals ~", paste(available_predictors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = numeric_df)
    
    # Get summary
    model_summary <- summary(model)
    
    message("Regression Model: Predicting Meeting Learning Goals\n")
    message("R-squared: ", round(model_summary$r.squared, 3))
    message("Adjusted R-squared: ", round(model_summary$adj.r.squared, 3))
    f_pvalue <- pf(model_summary$fstatistic[1],
                   model_summary$fstatistic[2],
                   model_summary$fstatistic[3], lower = FALSE)
    message("F-statistic: ", round(model_summary$fstatistic[1], 2),
            ", p-value: ", format(f_pvalue, scientific = TRUE, digits = 4), "\n")
    
    # Extract coefficients
    coef_df <- data.frame(
      Predictor = rownames(model_summary$coefficients),
      Estimate = round(model_summary$coefficients[, 1], 3),
      Std_Error = round(model_summary$coefficients[, 2], 3),
      T_Value = round(model_summary$coefficients[, 3], 3),
      P_Value = round(model_summary$coefficients[, 4], 4)
    )
    
    # Sort by absolute estimate
    coef_df$Abs_Estimate <- abs(coef_df$Estimate)
    coef_df <- coef_df[order(-coef_df$Abs_Estimate), ]
    
    message("Top predictors (by coefficient magnitude):\n")
    print(coef_df[coef_df$P_Value < 0.05, c("Predictor", "Estimate", "P_Value")], 
          row.names = FALSE)
    
    # Calculate relative importance (standardized coefficients) manually
    # Standardized coefficients = coefficient * (sd(x) / sd(y))
    y_sd <- sd(numeric_df$meeting_goals, na.rm = TRUE)
    std_coefs <- sapply(names(coef(model))[-1], function(var) {
      if (var %in% names(numeric_df)) {
        coef(model)[var] * sd(numeric_df[[var]], na.rm = TRUE) / y_sd
      } else {
        NA
      }
    })
    
    standardized_coefs <- data.frame(
      Predictor = names(std_coefs),
      Std_Coefficient = round(std_coefs, 3)
    )
    standardized_coefs$Abs_Std_Coeff <- abs(standardized_coefs$Std_Coefficient)
    standardized_coefs <- standardized_coefs[order(-standardized_coefs$Abs_Std_Coeff), ]
    
    message("\nStandardized coefficients (relative importance):\n")
    print(head(standardized_coefs, 10), row.names = FALSE)
    
    return(list(model = model, coefficients = coef_df, 
                standardized = standardized_coefs))
  }
  
  return(NULL)
}

# =============================================================================
# INSIGHT 3: CLUSTER ANALYSIS - Identify learner types
# =============================================================================

analyze_clusters <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 3: CLUSTER ANALYSIS - LEARNER TYPES")
  message("========================================\n")
  
  # Select variables for clustering
  cluster_vars <- c("pre_written_code", "live_coding", "slides", "handouts", 
                    "tophat_quizzes", "assignments", "labs", "ask_questions",
                    "studying_midterms", "coding_own", "friends_important",
                    "easy_meet", "part_of_class", "comfortable_speaking")
  
  available_vars <- cluster_vars[cluster_vars %in% names(numeric_df)]
  
  # Prepare data (remove rows with NA)
  cluster_data <- numeric_df[, available_vars]
  cluster_data <- na.omit(cluster_data)
  
  # Scale the data
  cluster_data_scaled <- scale(cluster_data)
  
  # Determine optimal number of clusters using silhouette method
  message("Determining optimal number of clusters...")
  
  silhouette_scores <- data.frame(k = 2:10, score = NA)
  
  for (k in 2:10) {
    set.seed(123)
    km <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(cluster_data_scaled))
    silhouette_scores$score[silhouette_scores$k == k] <- mean(sil[, 3])
  }
  
  optimal_k <- silhouette_scores$k[which.max(silhouette_scores$score)]
  message("Optimal number of clusters: ", optimal_k, "\n")
  
  # Perform final clustering
  set.seed(123)
  final_km <- kmeans(cluster_data_scaled, centers = optimal_k, nstart = 25)
  
  # Add cluster assignments to data
  numeric_df$cluster <- factor(final_km$cluster)
  
  # Calculate cluster profiles
  cluster_profiles <- aggregate(cluster_data, by = list(cluster = final_km$cluster), FUN = mean)
  
  message("Cluster profiles:\n")
  print(cluster_profiles, row.names = FALSE)
  
  # Calculate cluster sizes
  cluster_sizes <- table(final_km$cluster)
  message("\nCluster sizes:\n")
  print(cluster_sizes)
  
  # Compare clusters on learning outcomes
  if ("meeting_goals" %in% names(numeric_df)) {
    message("\nMeeting learning goals by cluster:\n")
    cluster_outcomes <- aggregate(numeric_df$meeting_goals, 
                                   by = list(cluster = numeric_df$cluster), 
                                   FUN = function(x) c(mean = mean(x), sd = sd(x)))
    print(cluster_outcomes, row.names = FALSE)
  }
  
  # Create cluster visualization
  fviz_cluster(final_km, data = cluster_data_scaled,
               geom = "point",
               ellipse.type = "convex",
               ggtheme = theme_bw(),
               main = paste("K-means Clustering (k =", optimal_k, ")"))
  
  return(list(clusters = final_km, profiles = cluster_profiles, 
              sizes = cluster_sizes, optimal_k = optimal_k))
}

# =============================================================================
# INSIGHT 4: ANOVA - Section differences
# =============================================================================

analyze_section_differences <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 4: SECTION DIFFERENCES (ANOVA)")
  message("========================================\n")
  
  # Get valid sections
  valid_sections <- numeric_df$section[!is.na(numeric_df$section)]
  numeric_df_filtered <- numeric_df[!is.na(numeric_df$section), ]
  
  # Variables to test
  test_vars <- c("meeting_goals", "apply_scenario", "excited", "relevant",
                 "pre_written_code", "live_coding", "coding_own", "part_of_class")
  
  available_vars <- test_vars[test_vars %in% names(numeric_df_filtered)]
  
  anova_results <- data.frame()
  
  for (var in available_vars) {
    # Perform ANOVA
    formula_str <- paste(var, "~ section")
    anova_model <- aov(as.formula(formula_str), data = numeric_df_filtered)
    anova_summary <- summary(anova_model)
    
    # Extract results
    f_value <- anova_summary[[1]]$`F value`[1]
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    anova_results <- rbind(anova_results, data.frame(
      Variable = var,
      F_Value = round(f_value, 3),
      P_Value = round(p_value, 4),
      Significant = p_value < 0.05
    ))
    
    # If significant, do post-hoc Tukey test
    if (p_value < 0.05) {
      tukey <- TukeyHSD(anova_model)
      message("\nSignificant differences found for: ", var)
      message("Tukey HSD results:\n")
      print(tukey)
    }
  }
  
  message("\nANOVA Summary:\n")
  print(anova_results, row.names = FALSE)
  
  return(anova_results)
}

# =============================================================================
# INSIGHT 5: EFFECT SIZE ANALYSIS - What matters most?
# =============================================================================

analyze_effect_sizes <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 5: EFFECT SIZE ANALYSIS")
  message("========================================\n")
  
  # Calculate effect sizes (Cohen's d) for high vs low performers
  if ("meeting_goals" %in% names(numeric_df)) {
    # Define high and low performers (top and bottom quartiles)
    q75 <- quantile(numeric_df$meeting_goals, 0.75, na.rm = TRUE)
    q25 <- quantile(numeric_df$meeting_goals, 0.25, na.rm = TRUE)
    
    high_performers <- numeric_df[numeric_df$meeting_goals >= q75, ]
    low_performers <- numeric_df[numeric_df$meeting_goals <= q25, ]
    
    message("High performers (top 25%): ", nrow(high_performers), " students")
    message("Low performers (bottom 25%): ", nrow(low_performers), " students\n")
    
    # Variables to compare
    compare_vars <- c("pre_written_code", "live_coding", "slides", "handouts", 
                      "tophat_quizzes", "assignments", "labs", "ask_questions",
                      "studying_midterms", "coding_own", "friends_important",
                      "easy_meet", "part_of_class", "comfortable_speaking")
    
    available_vars <- compare_vars[compare_vars %in% names(numeric_df)]
    
    effect_sizes <- data.frame()
    
    for (var in available_vars) {
      high_vals <- high_performers[[var]]
      low_vals <- low_performers[[var]]
      
      # Remove NA
      high_vals <- high_vals[!is.na(high_vals)]
      low_vals <- low_vals[!is.na(low_vals)]
      
      if (length(high_vals) > 0 && length(low_vals) > 0) {
        # Calculate Cohen's d
        pooled_sd <- sqrt(((length(high_vals) - 1) * var(high_vals) + 
                           (length(low_vals) - 1) * var(low_vals)) / 
                          (length(high_vals) + length(low_vals) - 2))
        
        cohens_d <- (mean(high_vals) - mean(low_vals)) / pooled_sd
        
        effect_sizes <- rbind(effect_sizes, data.frame(
          Variable = var,
          High_Mean = round(mean(high_vals), 3),
          Low_Mean = round(mean(low_vals), 3),
          Difference = round(mean(high_vals) - mean(low_vals), 3),
          Cohens_D = round(cohens_d, 3),
          Effect_Size = ifelse(abs(cohens_d) < 0.2, "Negligible",
                               ifelse(abs(cohens_d) < 0.5, "Small",
                                      ifelse(abs(cohens_d) < 0.8, "Medium", "Large")))
        ))
      }
    }
    
    # Sort by absolute Cohen's d
    effect_sizes$Abs_Cohens_D <- abs(effect_sizes$Cohens_D)
    effect_sizes <- effect_sizes[order(-effect_sizes$Abs_Cohens_D), ]
    
    message("Effect sizes (Cohen's d) between high and low performers:\n")
    print(effect_sizes[, c("Variable", "High_Mean", "Low_Mean", "Cohens_D", "Effect_Size")], 
          row.names = FALSE)
    
    return(effect_sizes)
  }
  
  return(NULL)
}

# =============================================================================
# INSIGHT 6: RELIABILITY ANALYSIS - Internal consistency
# =============================================================================

analyze_reliability <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 6: RELIABILITY ANALYSIS")
  message("========================================\n")
  
  # Calculate Cronbach's alpha for each category
  categories <- list(
    course = c("excited", "relevant", "meeting_goals", "apply_scenario", "feedback", "ask_help"),
    learning = c("pre_written_code", "live_coding", "slides", "handouts", 
                 "tophat_quizzes", "assignments", "labs", "ask_questions",
                 "studying_midterms", "coding_own"),
    community = c("friends_important", "easy_meet", "part_of_class", 
                  "comfortable_speaking", "part_of_university")
  )
  
  reliability_results <- data.frame()
  
  for (cat_name in names(categories)) {
    vars <- categories[[cat_name]]
    available_vars <- vars[vars %in% names(numeric_df)]
    
    if (length(available_vars) >= 2) {
      cat_data <- numeric_df[, available_vars]
      cat_data <- na.omit(cat_data)
      
      # Calculate Cronbach's alpha
      alpha_result <- psych::alpha(cat_data)
      
      reliability_results <- rbind(reliability_results, data.frame(
        Category = cat_name,
        Cronbach_Alpha = round(alpha_result$total$raw_alpha, 3),
        Std_Alpha = round(alpha_result$total$std.alpha, 3),
        Average_R = round(alpha_result$total$average_r, 3),
        N_Items = length(available_vars)
      ))
      
      message(cat_name, " Cronbach's alpha: ", round(alpha_result$total$raw_alpha, 3))
      
      # Show item-total correlations
      if (nrow(alpha_result$item.stats) > 0) {
        message("  Item-total correlations:\n")
        item_corr <- alpha_result$item.stats[, c("raw.r", "std.r")]
        rownames(item_corr) <- available_vars
        print(round(item_corr, 3))
      }
    }
  }
  
  message("\nReliability Summary:\n")
  print(reliability_results, row.names = FALSE)
  
  return(reliability_results)
}

# =============================================================================
# INSIGHT 7: INTERACTION ANALYSIS - Do learning methods work differently?
# =============================================================================

analyze_interactions <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 7: INTERACTION ANALYSIS")
  message("========================================\n")
  
  # Test if the relationship between learning methods and outcomes
  # differs based on community connection
  
  if ("meeting_goals" %in% names(numeric_df) && "part_of_class" %in% names(numeric_df)) {
    # Create high/low community connection groups
    median_community <- median(numeric_df$part_of_class, na.rm = TRUE)
    numeric_df$community_group <- ifelse(numeric_df$part_of_class >= median_community, 
                                          "High", "Low")
    numeric_df$community_group <- factor(numeric_df$community_group)
    
    # Test interaction for key learning methods
    learning_methods <- c("coding_own", "assignments", "live_coding", "pre_written_code")
    available_methods <- learning_methods[learning_methods %in% names(numeric_df)]
    
    interaction_results <- data.frame()
    
    for (method in available_methods) {
      formula_str <- paste("meeting_goals ~", method, "* community_group")
      model <- lm(as.formula(formula_str), data = numeric_df)
      model_summary <- summary(model)
      
      # Extract interaction p-value
      coef_names <- rownames(model_summary$coefficients)
      interaction_coef <- coef_names[grepl(":", coef_names)]
      
      if (length(interaction_coef) > 0) {
        interaction_p <- model_summary$coefficients[interaction_coef, 4]
        
        interaction_results <- rbind(interaction_results, data.frame(
          Learning_Method = method,
          Interaction_P_Value = round(interaction_p, 4),
          Significant_Interaction = interaction_p < 0.05
        ))
        
        if (interaction_p < 0.05) {
          message("Significant interaction found: ", method, " × community connection")
          
          # Plot interaction
          interaction_plot <- ggplot(numeric_df, aes_string(x = method, y = "meeting_goals", 
                                                             color = "community_group")) +
            geom_smooth(method = "lm", se = TRUE) +
            geom_point(alpha = 0.3) +
            labs(title = paste("Interaction:", method, "× Community Connection"),
                 x = method, y = "Meeting Learning Goals", color = "Community Connection") +
            theme_minimal()
          
          print(interaction_plot)
        }
      }
    }
    
    message("\nInteraction Analysis Results:\n")
    print(interaction_results, row.names = FALSE)
    
    return(interaction_results)
  }
  
  return(NULL)
}

# =============================================================================
# INSIGHT 8: PREDICTIVE MODELING - What predicts student satisfaction?
# =============================================================================

analyze_satisfaction_predictors <- function(numeric_df) {
  message("\n========================================")
  message("INSIGHT 8: SATISFACTION PREDICTORS")
  message("========================================\n")
  
  # Use feedback satisfaction as outcome
  if ("feedback" %in% names(numeric_df)) {
    # Create binary satisfaction variable (satisfied = 4 or 5)
    numeric_df$satisfied <- ifelse(numeric_df$feedback >= 4, 1, 0)
    
    # Predictors
    predictors <- c("excited", "relevant", "meeting_goals", "apply_scenario",
                    "ask_help", "pre_written_code", "live_coding", "coding_own",
                    "part_of_class", "comfortable_speaking")
    
    available_preds <- predictors[predictors %in% names(numeric_df)]
    
    # Logistic regression
    formula_str <- paste("satisfied ~", paste(available_preds, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = numeric_df, family = binomial())
    
    model_summary <- summary(logit_model)
    
    message("Logistic Regression: Predicting Satisfaction with Feedback\n")
    message("AIC: ", round(logit_model$aic, 2), "\n")
    
    # Extract coefficients
    coef_df <- data.frame(
      Predictor = rownames(model_summary$coefficients),
      Odds_Ratio = round(exp(model_summary$coefficients[, 1]), 3),
      CI_Lower = round(exp(model_summary$coefficients[, 1] - 1.96 * model_summary$coefficients[, 2]), 3),
      CI_Upper = round(exp(model_summary$coefficients[, 1] + 1.96 * model_summary$coefficients[, 2]), 3),
      P_Value = round(model_summary$coefficients[, 4], 4)
    )
    
    # Sort by odds ratio
    coef_df <- coef_df[order(-coef_df$Odds_Ratio), ]
    
    message("Top predictors of satisfaction (by odds ratio):\n")
    print(coef_df[coef_df$P_Value < 0.05, c("Predictor", "Odds_Ratio", "CI_Lower", "CI_Upper", "P_Value")], 
          row.names = FALSE)
    
    # Model accuracy
    predicted <- ifelse(predict(logit_model, type = "response") > 0.5, 1, 0)
    accuracy <- mean(predicted == numeric_df$satisfied, na.rm = TRUE)
    message("\nModel accuracy: ", round(accuracy * 100, 1), "%")
    
    return(list(model = logit_model, coefficients = coef_df, accuracy = accuracy))
  }
  
  return(NULL)
}

# =============================================================================
# GENERATE INSIGHTS REPORT
# =============================================================================

generate_insights_report <- function(results) {
  message("\n========================================")
  message("KEY INSTRUCTIONAL DESIGN INSIGHTS")
  message("========================================\n")
  
  message("1. LEARNING METHOD EFFECTIVENESS")
  message("   Based on correlation and regression analysis:\n")
  
  if (!is.null(results$regression$standardized)) {
    top_predictors <- head(results$regression$standardized, 5)
    for (i in 1:nrow(top_predictors)) {
      message("   - ", top_predictors$Predictor[i], 
              " (β = ", top_predictors$Std_Coefficient[i], ")")
    }
  }
  
  message("\n2. LEARNER SEGMENTATION")
  message("   Based on cluster analysis:\n")
  if (!is.null(results$clusters)) {
    message("   - ", results$clusters$optimal_k, " distinct learner types identified")
    message("   - Cluster sizes: ", paste(results$clusters$sizes, collapse = ", "))
  }
  
  message("\n3. HIGH vs LOW PERFORMER DIFFERENCES")
  message("   Based on effect size analysis:\n")
  if (!is.null(results$effect_sizes)) {
    large_effects <- results$effect_sizes[results$effect_sizes$Effect_Size == "Large", ]
    if (nrow(large_effects) > 0) {
      for (i in 1:min(3, nrow(large_effects))) {
        message("   - ", large_effects$Variable[i], 
                " (d = ", large_effects$Cohens_D[i], ")")
      }
    }
  }
  
  message("\n4. COMMUNITY CONNECTION IMPACT")
  message("   Based on correlation analysis:\n")
  if (!is.null(results$correlations$correlations)) {
    community_corrs <- results$correlations$correlations[
      results$correlations$correlations$Predictor %in% 
        c("part_of_class", "comfortable_speaking", "easy_meet"), ]
    if (nrow(community_corrs) > 0) {
      for (i in 1:nrow(community_corrs)) {
        if (community_corrs$Significant[i]) {
          message("   - ", community_corrs$Predictor[i], " → ", 
                  community_corrs$Outcome[i], 
                  " (r = ", community_corrs$Correlation[i], ")")
        }
      }
    }
  }
  
  message("\n5. SATISFACTION DRIVERS")
  message("   Based on logistic regression:\n")
  if (!is.null(results$satisfaction$coefficients)) {
    top_odds <- head(results$satisfaction$coefficients[
      results$satisfaction$coefficients$P_Value < 0.05, ], 3)
    if (nrow(top_odds) > 0) {
      for (i in 1:nrow(top_odds)) {
        message("   - ", top_odds$Predictor[i], 
                " (OR = ", top_odds$Odds_Ratio[i], ")")
      }
    }
  }
  
  message("\n========================================\n")
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Load and prepare data
df <- load_data()
numeric_df <- prepare_analysis_data(df)

# Run all analyses
results <- list()

results$correlations <- analyze_correlations(numeric_df)
results$regression <- analyze_regression(numeric_df)
results$clusters <- analyze_clusters(numeric_df)
results$anova <- analyze_section_differences(numeric_df)
results$effect_sizes <- analyze_effect_sizes(numeric_df)
results$reliability <- analyze_reliability(numeric_df)
results$interactions <- analyze_interactions(numeric_df)
results$satisfaction <- analyze_satisfaction_predictors(numeric_df)

# Generate insights report
generate_insights_report(results)

message("\n========================================")
message("ADVANCED EDA ANALYSIS COMPLETE")
message("========================================\n")
