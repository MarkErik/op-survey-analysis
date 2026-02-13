# Statistical Calculations Module
# Functions for performing statistical analysis on survey data

#' Calculate Descriptive Statistics
#'
#' Computes descriptive statistics for a numeric column including mean, median,
#' mode, standard deviation, quartiles, and range.
#'
#' @param df A data frame containing the data
#' @param column The column name to analyze
#' @return A list with descriptive statistics or NULL if invalid
#' @export
calculate_descriptive_stats <- function(df, column) {
  if (is.null(df) || !column %in% names(df)) {
    return(NULL)
  }
  
  values <- df[[column]]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  # Calculate mode
  freq_table <- table(values)
  mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
  
  list(
    n = length(values),
    mean = mean(values),
    median = median(values),
    mode = mode_val,
    sd = sd(values),
    min = min(values),
    q1 = quantile(values, 0.25),
    q3 = quantile(values, 0.75),
    max = max(values),
    range = max(values) - min(values),
    iqr = as.numeric(quantile(values, 0.75) - quantile(values, 0.25)),
    cv = ifelse(mean(values) != 0, sd(values) / abs(mean(values)) * 100, NA)
  )
}

#' Calculate Correlation Matrix
#'
#' Computes a correlation matrix for specified numeric columns.
#' Uses Pearson correlation by default, with option for Spearman.
#'
#' @param df A data frame containing the data
#' @param columns A character vector of column names to include
#' @param method Correlation method: "pearson" or "spearman"
#' @return A correlation matrix or NULL if invalid
#' @export
calculate_correlation_matrix <- function(df, columns, method = "pearson") {
  if (is.null(df) || length(columns) < 2) {
    return(NULL)
  }
  
  # Filter to existing columns
  existing_cols <- intersect(columns, names(df))
  if (length(existing_cols) < 2) {
    return(NULL)
  }
  
  # Extract numeric data
  data_subset <- df[, existing_cols, drop = FALSE]
  
  # Convert to numeric if needed
  for (col in names(data_subset)) {
    if (!is.numeric(data_subset[[col]])) {
      data_subset[[col]] <- as.numeric(data_subset[[col]])
    }
  }
  
  # Remove rows with NA values
  data_subset <- data_subset[complete.cases(data_subset), ]
  
  if (nrow(data_subset) < 2) {
    return(NULL)
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_subset, method = method, use = "complete.obs")
  
  return(cor_matrix)
}

#' Perform ANOVA Test
#'
#' Performs one-way ANOVA to compare means across groups.
#'
#' @param df A data frame containing the data
#' @param value_col The column name with numeric values
#' @param group_col The column name with group categories
#' @return A list with ANOVA results or NULL if invalid
#' @export
perform_anova_test <- function(df, value_col, group_col) {
  if (is.null(df) || !value_col %in% names(df) || !group_col %in% names(df)) {
    return(NULL)
  }
  
  # Remove NA values
  data_subset <- df[, c(value_col, group_col), drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), ]
  
  if (nrow(data_subset) < 2) {
    return(NULL)
  }
  
  # Ensure value column is numeric
  if (!is.numeric(data_subset[[value_col]])) {
    data_subset[[value_col]] <- as.numeric(data_subset[[value_col]])
  }
  
  # Check if we have at least 2 groups with data
  groups <- unique(data_subset[[group_col]])
  if (length(groups) < 2) {
    return(NULL)
  }
  
  # Perform ANOVA
  formula <- as.formula(paste(value_col, "~", group_col))
  
  tryCatch({
    anova_result <- aov(formula, data = data_subset)
    anova_summary <- summary(anova_result)
    
    # Extract p-value
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    # Calculate effect size (eta-squared)
    ss_between <- anova_summary[[1]]$`Sum Sq`[1]
    ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
    eta_squared <- ss_between / ss_total
    
    # Group means
    group_means <- aggregate(
      data_subset[[value_col]], 
      by = list(group = data_subset[[group_col]]), 
      FUN = mean
    )
    names(group_means) <- c("group", "mean")
    
    list(
      f_statistic = anova_summary[[1]]$`F value`[1],
      p_value = p_value,
      significant = p_value < 0.05,
      eta_squared = eta_squared,
      group_means = group_means,
      degrees_of_freedom = list(
        between = anova_summary[[1]]$Df[1],
        within = anova_summary[[1]]$Df[2]
      )
    )
  }, error = function(e) {
    NULL
  })
}

#' Perform Chi Square Test
#'
#' Performs chi-square test of independence for categorical variables.
#'
#' @param df A data frame containing the data
#' @param col1 First categorical column name
#' @param col2 Second categorical column name
#' @return A list with chi-square test results or NULL if invalid
#' @export
perform_chi_square_test <- function(df, col1, col2) {
  if (is.null(df) || !col1 %in% names(df) || !col2 %in% names(df)) {
    return(NULL)
  }
  
  # Create contingency table
  contingency_table <- table(df[[col1]], df[[col2]], useNA = "no")
  
  # Check if table is valid
  if (sum(contingency_table) < 5) {
    return(NULL)
  }
  
  # Perform chi-square test
  tryCatch({
    chi_result <- chisq.test(contingency_table)
    
    # Calculate Cramer's V (effect size)
    n <- sum(contingency_table)
    min_dim <- min(dim(contingency_table) - 1)
    cramers_v <- sqrt(chi_result$statistic / (n * min_dim))
    
    list(
      chi_square = as.numeric(chi_result$statistic),
      p_value = chi_result$p.value,
      significant = chi_result$p.value < 0.05,
      degrees_of_freedom = chi_result$parameter,
      cramers_v = cramers_v,
      contingency_table = contingency_table,
      expected_counts = chi_result$expected
    )
  }, error = function(e) {
    NULL
  })
}

#' Calculate Effect Size (Cohen's d)
#'
#' Calculates Cohen's d effect size for comparing two groups.
#'
#' @param df A data frame containing the data
#' @param value_col The column name with numeric values
#' @param group_col The column name with group categories
#' @param group1 Value of first group
#' @param group2 Value of second group
#' @return A list with effect size statistics or NULL if invalid
#' @export
calculate_effect_size <- function(df, value_col, group_col, group1, group2) {
  if (is.null(df) || !value_col %in% names(df) || !group_col %in% names(df)) {
    return(NULL)
  }
  
  # Extract data for each group
  group1_data <- df[[value_col]][df[[group_col]] == group1]
  group2_data <- df[[value_col]][df[[group_col]] == group2]
  
  # Remove NA values
  group1_data <- group1_data[!is.na(group1_data)]
  group2_data <- group2_data[!is.na(group2_data)]
  
  if (length(group1_data) < 2 || length(group2_data) < 2) {
    return(NULL)
  }
  
  # Ensure numeric
  group1_data <- as.numeric(group1_data)
  group2_data <- as.numeric(group2_data)
  
  # Calculate means and standard deviations
  mean1 <- mean(group1_data)
  mean2 <- mean(group2_data)
  sd1 <- sd(group1_data)
  sd2 <- sd(group2_data)
  n1 <- length(group1_data)
  n2 <- length(group2_data)
  
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  
  # Cohen's d
  cohens_d <- (mean1 - mean2) / pooled_sd
  
  # Interpret effect size
  interpretation <- case_when(
    abs(cohens_d) < 0.2 ~ "negligible",
    abs(cohens_d) < 0.5 ~ "small",
    abs(cohens_d) < 0.8 ~ "medium",
    TRUE ~ "large"
  )
  
  list(
    cohens_d = cohens_d,
    interpretation = interpretation,
    mean_difference = mean1 - mean2,
    pooled_sd = pooled_sd,
    group1 = list(
      name = group1,
      n = n1,
      mean = mean1,
      sd = sd1
    ),
    group2 = list(
      name = group2,
      n = n2,
      mean = mean2,
      sd = sd2
    )
  )
}

#' Calculate Group Comparison Statistics
#'
#' Performs comprehensive statistical comparison between groups.
#'
#' @param df A data frame containing the data
#' @param value_col The column name with numeric values
#' @param group_col The column name with group categories
#' @return A list with comparison statistics or NULL if invalid
#' @export
calculate_group_comparison <- function(df, value_col, group_col) {
  if (is.null(df) || !value_col %in% names(df) || !group_col %in% names(df)) {
    return(NULL)
  }
  
  # Get unique groups
  groups <- unique(df[[group_col]])
  groups <- groups[!is.na(groups)]
  
  if (length(groups) < 2) {
    return(NULL)
  }
  
  # Calculate descriptive stats for each group
  group_stats <- lapply(groups, function(g) {
    group_data <- df[[value_col]][df[[group_col]] == g]
    group_data <- group_data[!is.na(group_data)]
    
    if (length(group_data) > 0) {
      list(
        group = g,
        n = length(group_data),
        mean = mean(group_data),
        median = median(group_data),
        sd = sd(group_data),
        min = min(group_data),
        max = max(group_data)
      )
    } else {
      NULL
    }
  })
  group_stats <- Filter(Negate(is.null), group_stats)
  
  # Perform ANOVA if more than 2 groups
  anova_result <- NULL
  if (length(groups) >= 2) {
    anova_result <- perform_anova_test(df, value_col, group_col)
  }
  
  # Calculate pairwise effect sizes
  pairwise_effects <- list()
  if (length(groups) == 2) {
    pairwise_effects[[paste(groups[1], "vs", groups[2])]] <- 
      calculate_effect_size(df, value_col, group_col, groups[1], groups[2])
  } else if (length(groups) > 2) {
    for (i in 1:(length(groups) - 1)) {
      for (j in (i + 1):length(groups)) {
        key <- paste(groups[i], "vs", groups[j])
        pairwise_effects[[key]] <- calculate_effect_size(df, value_col, group_col, groups[i], groups[j])
      }
    }
  }
  
  list(
    value_column = value_col,
    group_column = group_col,
    group_statistics = group_stats,
    anova = anova_result,
    pairwise_effects = pairwise_effects
  )
}

#' Calculate Likert Scale Statistics
#'
#' Calculates statistics specific to Likert scale data including
#-- distribution percentages and agreement levels.
#'
#' @param df A data frame containing the data
#' @param column The Likert scale column name
#' @return A list with Likert statistics or NULL if invalid
#' @export
calculate_likert_stats <- function(df, column) {
  if (is.null(df) || !column %in% names(df)) {
    return(NULL)
  }
  
  values <- df[[column]]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  # Define Likert labels
  likert_labels <- c("1" = "Strongly Disagree", "2" = "Disagree",
                     "3" = "Neutral", "4" = "Agree", "5" = "Strongly Agree")
  
  # Count responses
  response_counts <- table(factor(values, levels = 1:5))
  response_df <- data.frame(
    response = names(likert_labels),
    label = likert_labels,
    count = as.numeric(response_counts),
    percentage = round(as.numeric(response_counts) / sum(response_counts) * 100, 1)
  )
  
  # Calculate agreement metrics
  agree_count <- sum(values >= 4, na.rm = TRUE)
  disagree_count <- sum(values <= 2, na.rm = TRUE)
  neutral_count <- sum(values == 3, na.rm = TRUE)
  total <- length(values)
  
  # Net agreement score (agree - disagree)
  net_agreement <- (agree_count - disagree_count) / total * 100
  
  # Weighted mean (1-5 scale)
  weighted_mean <- mean(values, na.rm = TRUE)
  
  list(
    n = total,
    mean = weighted_mean,
    median = median(values),
    sd = sd(values),
    distribution = response_df,
    agree_percentage = round(agree_count / total * 100, 1),
    disagree_percentage = round(disagree_count / total * 100, 1),
    neutral_percentage = round(neutral_count / total * 100, 1),
    net_agreement = round(net_agreement, 1),
    consensus_level = case_when(
      sd(values) < 0.8 ~ "High",
      sd(values) < 1.2 ~ "Moderate",
      TRUE ~ "Low"
    )
  )
}
