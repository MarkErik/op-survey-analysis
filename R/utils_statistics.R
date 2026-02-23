# Statistical Calculation Functions
# Helper functions for statistical analysis
#
# @author Course Instructor
# @version 2.0.0

#' Calculate descriptive statistics
#'
#' Computes comprehensive descriptive statistics for a variable
#'
#' @param x Numeric vector
#' @return List of statistics
#' @export
calculate_descriptive_stats <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
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
      iqr = NA,
      skewness = NA,
      kurtosis = NA
    ))
  }

  n <- length(x)
  mean_val <- mean(x)
  median_val <- median(x)
  mode_val <- as.integer(names(which.max(table(x))))
  sd_val <- sd(x)
  se_val <- sd_val / sqrt(n)
  min_val <- min(x)
  max_val <- max(x)
  q1_val <- quantile(x, 0.25)
  q3_val <- quantile(x, 0.75)
  iqr_val <- q3_val - q1_val

  # Calculate skewness
  if (sd_val > 0) {
    skewness_val <- mean((x - mean_val)^3) / sd_val^3
  } else {
    skewness_val <- 0
  }

  # Calculate kurtosis
  if (sd_val > 0) {
    kurtosis_val <- mean((x - mean_val)^4) / sd_val^4 - 3
  } else {
    kurtosis_val <- 0
  }

  list(
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
    iqr = iqr_val,
    skewness = skewness_val,
    kurtosis = kurtosis_val
  )
}

#' Calculate confidence interval
#'
#' Computes confidence interval for the mean
#'
#' @param x Numeric vector
#' @param confidence Confidence level (default 0.95)
#' @return Vector with lower and upper bounds
#' @export
calculate_ci <- function(x, confidence = 0.95) {
  x <- x[!is.na(x)]

  if (length(x) < 2) {
    return(c(lower = NA, upper = NA))
  }

  n <- length(x)
  mean_val <- mean(x)
  se <- sd(x) / sqrt(n)

  # t-critical value
  t_crit <- qt((1 + confidence) / 2, df = n - 1)

  c(
    lower = mean_val - t_crit * se,
    upper = mean_val + t_crit * se
  )
}

#' Perform t-test
#'
#' Conducts independent samples t-test
#'
#' @param group1 Numeric vector for group 1
#' @param group2 Numeric vector for group 2
#' @param alternative Alternative hypothesis ("two.sided", "less", "greater")
#' @return List with test results
#' @export
perform_ttest <- function(group1, group2, alternative = "two.sided") {
  group1 <- group1[!is.na(group1)]
  group2 <- group2[!is.na(group2)]

  if (length(group1) < 2 || length(group2) < 2) {
    return(list(
      statistic = NA,
      p_value = NA,
      df = NA,
      conf_int = c(NA, NA),
      effect_size = NA
    ))
  }

  test_result <- t.test(group1, group2, alternative = alternative)

  # Calculate Cohen's d
  pooled_sd <- sqrt(((length(group1) - 1) * sd(group1)^2 + (length(group2) - 1) * sd(group2)^2) /
                      (length(group1) + length(group2) - 2))
  cohens_d <- (mean(group1) - mean(group2)) / pooled_sd

  list(
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    df = test_result$parameter,
    conf_int = test_result$conf.int,
    effect_size = cohens_d,
    mean_diff = mean(group1) - mean(group2)
  )
}

#' Perform ANOVA
#'
#' Conducts one-way ANOVA
#'
#' @param data Data frame
#' @param dv Dependent variable column
#' @param iv Independent variable (grouping) column
#' @return List with ANOVA results
#' @export
perform_anova <- function(data, dv, iv) {
  if (!dv %in% colnames(data) || !iv %in% colnames(data)) {
    return(NULL)
  }

  # Remove missing values
  analysis_data <- data[, c(dv, iv)] %>%
    dplyr::filter(!is.na(.data[[dv]]) & !is.na(.data[[iv]]))

  if (nrow(analysis_data) < 10) {
    return(NULL)
  }

  # Perform ANOVA
  model <- aov(as.formula(paste(dv, "~", iv)), data = analysis_data)
  anova_result <- summary(model)[[1]]

  # Calculate effect size (eta-squared)
  ss_between <- anova_result$"Sum Sq"[1]
  ss_total <- sum(anova_result$"Sum Sq")
  eta_squared <- ss_between / ss_total

  list(
    f_statistic = anova_result$"F value"[1],
    p_value = anova_result$"Pr(>F)"[1],
    df_between = anova_result$Df[1],
    df_within = anova_result$Df[2],
    ss_between = ss_between,
    ss_within = anova_result$"Sum Sq"[2],
    eta_squared = eta_squared,
    significant = anova_result$"Pr(>F)"[1] < 0.05
  )
}

#' Perform chi-square test
#'
#' Conducts chi-square test of independence
#'
#' @param data Data frame
#' @param var1 First variable
#' @param var2 Second variable
#' @return List with test results
#' @export
perform_chi_square <- function(data, var1, var2) {
  if (!var1 %in% colnames(data) || !var2 %in% colnames(data)) {
    return(NULL)
  }

  # Create contingency table
  contingency <- table(data[[var1]], data[[var2]])

  if (any(contingency < 5)) {
    warning("Some cells have expected counts < 5. Consider using Fisher's exact test.")
  }

  test_result <- chisq.test(contingency)

  # Calculate Cramér's V
  n <- sum(contingency)
  min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
  cramers_v <- sqrt(test_result$statistic / (n * min_dim))

  list(
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    df = test_result$parameter,
    cramers_v = cramers_v,
    observed = contingency,
    expected = test_result$expected
  )
}

#' Calculate correlation with significance
#'
#' Computes correlation and p-value
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return List with correlation results
#' @export
calculate_correlation <- function(x, y, method = "pearson") {
  # Remove pairs with missing values
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]

  if (length(x) < 3) {
    return(list(
      correlation = NA,
      p_value = NA,
      n = length(x),
      method = method
    ))
  }

  test_result <- cor.test(x, y, method = method)

  list(
    correlation = test_result$estimate,
    p_value = test_result$p.value,
    conf_int = test_result$conf.int,
    n = length(x),
    method = method
  )
}

#' Calculate Cronbach's alpha
#'
#' Computes internal consistency reliability
#'
#' @param data Data frame with items
#' @return Cronbach's alpha value
#' @export
calculate_cronbach_alpha <- function(data) {
  data <- na.omit(data)

  n_items <- ncol(data)
  n_respondents <- nrow(data)

  if (n_items < 2 || n_respondents < 2) {
    return(NA)
  }

  # Calculate item variances
  item_variances <- sapply(data, var)

  # Calculate total score variance
  total_scores <- rowSums(data)
  total_variance <- var(total_scores)

  if (total_variance == 0) {
    return(NA)
  }

  # Calculate alpha
  alpha <- (n_items / (n_items - 1)) * (1 - sum(item_variances) / total_variance)

  return(alpha)
}

#' Perform factor analysis
#'
#' Conducts exploratory factor analysis
#'
#' @param data Data frame with variables
#' @param n_factors Number of factors to extract
#' @return List with factor analysis results
#' @export
perform_factor_analysis <- function(data, n_factors = 2) {
  data <- na.omit(data)

  if (nrow(data) < 10 || ncol(data) < n_factors) {
    return(NULL)
  }

  # Perform PCA as simple factor analysis
  pca_result <- prcomp(data, scale = TRUE)

  # Get loadings
  loadings <- pca_result$rotation[, 1:n_factors]

  # Calculate variance explained
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cumvar <- cumsum(var_explained)

  list(
    loadings = loadings,
    variance_explained = var_explained[1:n_factors],
    cumulative_variance = cumvar[n_factors],
    eigenvalues = pca_result$sdev^2
  )
}

#' Perform k-means clustering
#'
#' Conducts k-means clustering analysis
#'
#' @param data Data frame with variables
#' @param k Number of clusters
#' @param n_start Number of random starts
#' @return List with clustering results
#' @export
perform_clustering <- function(data, k = 3, n_start = 10) {
  data <- na.omit(data)

  if (nrow(data) < k * 2) {
    return(NULL)
  }

  # Standardize data
  data_scaled <- scale(data)

  # Perform k-means
  set.seed(123)
  result <- kmeans(data_scaled, centers = k, nstart = n_start)

  # Calculate cluster means
  cluster_means <- aggregate(data, by = list(cluster = result$cluster), mean, na.rm = TRUE)

  # Calculate silhouette score
  dist_matrix <- dist(data_scaled)
  silhouette_result <- cluster::silhouette(result$cluster, dist_matrix)
  avg_silhouette <- mean(silhouette_result[, 3])

  list(
    cluster = result$cluster,
    centers = result$centers,
    size = result$size,
    between_ss = result$betweenss,
    total_ss = result$totss,
    cluster_means = cluster_means,
    silhouette_score = avg_silhouette
  )
}

#' Calculate effect size (Cohen's d)
#'
#' Computes Cohen's d for effect size
#'
#' @param group1 Numeric vector for group 1
#' @param group2 Numeric vector for group 2
#' @return Cohen's d value
#' @export
calculate_effect_size_d <- function(group1, group2) {
  group1 <- group1[!is.na(group1)]
  group2 <- group2[!is.na(group2)]

  if (length(group1) < 2 || length(group2) < 2) {
    return(NA)
  }

  mean1 <- mean(group1)
  mean2 <- mean(group2)
  sd1 <- sd(group1)
  sd2 <- sd(group2)

  # Pooled standard deviation
  pooled_sd <- sqrt(((length(group1) - 1) * sd1^2 + (length(group2) - 1) * sd2^2) /
                      (length(group1) + length(group2) - 2))

  if (pooled_sd == 0) {
    return(0)
  }

  d <- (mean1 - mean2) / pooled_sd

  return(d)
}

#' Calculate effect size (Cohen's f)
#'
#' Computes Cohen's f for ANOVA effect size
#'
#' @param eta_squared Eta-squared value
#' @return Cohen's f value
#' @export
calculate_effect_size_f <- function(eta_squared) {
  if (is.na(eta_squared) || eta_squared >= 1) {
    return(NA)
  }

  sqrt(eta_squared / (1 - eta_squared))
}

#' Interpret effect size
#'
#' Provides interpretation of effect size
#'
#' @param d Cohen's d value
#' @return Character string with interpretation
#' @export
interpret_effect_size <- function(d) {
  abs_d <- abs(d)

  dplyr::case_when(
    abs_d < 0.2 ~ "Negligible",
    abs_d < 0.5 ~ "Small",
    abs_d < 0.8 ~ "Medium",
    TRUE ~ "Large"
  )
}

#' Perform post-hoc test
#'
#' Conducts Tukey HSD post-hoc test
#'
#' @param aov_result Result from aov()
#' @return Post-hoc test results
#' @export
perform_posthoc_test <- function(aov_result) {
  posthoc <- TukeyHSD(aov_result)

  # Extract results into data frame
  result_df <- as.data.frame(posthoc[[1]])
  result_df$comparison <- rownames(result_df)

  result_df
}

#' Calculate reliability statistics
#'
#' Computes various reliability metrics
#'
#' @param data Data frame with Likert responses
#' @return List of reliability statistics
#' @export
calculate_reliability <- function(data) {
  data <- na.omit(data)

  if (ncol(data) < 2 || nrow(data) < 10) {
    return(NULL)
  }

  # Cronbach's alpha
  alpha <- calculate_cronbach_alpha(data)

  # Split-half reliability
  n_items <- ncol(data)
  half1 <- ceiling(n_items / 2)
  half2 <- n_items - half1

  scores1 <- rowMeans(data[, 1:half1])
  scores2 <- rowMeans(data[, (half1 + 1):n_items])

  split_half <- cor(scores1, scores2)

  # Guttman lambda
  item_loadings <- psych::alpha(data)$alpha

  list(
    cronbach_alpha = alpha,
    split_half_reliability = split_half,
    n_items = n_items,
    n_respondents = nrow(data)
  )
}

#' Calculate non-parametric tests
#'
#' Performs Mann-Whitney U test
#'
#' @param group1 Numeric vector for group 1
#' @param group2 Numeric vector for group 2
#' @return List with test results
#' @export
perform_mann_whitney <- function(group1, group2) {
  group1 <- group1[!is.na(group1)]
  group2 <- group2[!is.na(group2)]

  if (length(group1) < 2 || length(group2) < 2) {
    return(NULL)
  }

  test_result <- wilcox.test(group1, group2)

  # Calculate rank-biserial correlation
  n1 <- length(group1)
  n2 <- length(group2)
  r <- 1 - (2 * test_result$statistic) / (n1 * n2)

  list(
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    r = r,
    n1 = n1,
    n2 = n2
  )
}

#' Calculate Kruskal-Wallis test
#'
#' Performs Kruskal-Wallis H test
#'
#' @param data Data frame
#' @param dv Dependent variable
#' @param iv Independent variable (grouping)
#' @return List with test results
#' @export
perform_kruskal_wallis <- function(data, dv, iv) {
  if (!dv %in% colnames(data) || !iv %in% colnames(data)) {
    return(NULL)
  }

  analysis_data <- data[, c(dv, iv)] %>%
    dplyr::filter(!is.na(.data[[dv]]) & !is.na(.data[[iv]]))

  if (nrow(analysis_data) < 10) {
    return(NULL)
  }

  test_result <- kruskal.test(as.formula(paste(dv, "~", iv)), data = analysis_data)

  list(
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    df = test_result$parameter
  )
}
