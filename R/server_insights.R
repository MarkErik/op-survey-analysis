# Insights Tab Server Logic
# Contains reactive logic for advanced statistical analysis
#
# @author Course Instructor
# @version 2.0.0

#' Insights Tab Server Function
#'
#' Contains all reactive logic for the Insights tab
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv Reactive values containing shared state
#' @export
server_insights <- function(input, output, session, rv) {

  # Reactive value for selected category
  selected_category <- reactiveVal("course_satisfaction")

  # Section filter display
  output$insights_section_filter_display <- renderUI({
    section <- rv$selected_section

    if (is.null(section) || section == "") {
      return(NULL)
    }

    div(
      class = "section-filter mb-3",
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px;",
      tags$strong("Filtered by: "),
      tags$span(class = "badge bg-primary", section),
      actionButton(
        inputId = "reset_insights_filter",
        label = "Reset",
        icon = icon("xmark"),
        class = "btn-sm btn-outline-danger",
        style = "margin-left: 10px;"
      )
    )
  })

  # Reset filter handler
  observeEvent(input$reset_insights_filter, {
    rv$selected_section <- NULL
    rv$current_data <- SURVEY_DATA
    updateSelectInput(session, "section_filter", selected = "")
  })

  # Category change handler
  observeEvent(input$insights_category, {
    selected_category(input$insights_category)
  })

  # Correlation matrix
  output$insights_correlation_matrix <- renderGirafe({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(NULL)
    }

    # Calculate correlation matrix
    cor_matrix <- cor(data[, existing_cols], use = "pairwise.complete.obs")

    # Convert to long format for plotting
    cor_df <- reshape2::melt(cor_matrix)
    colnames(cor_df) <- c("Var1", "Var2", "value")

    # Shorten variable names
    labels <- get_category_labels(category)
    cor_df$Var1 <- labels[match(cor_df$Var1, existing_cols)]
    cor_df$Var2 <- labels[match(cor_df$Var2, existing_cols)]

    # Create interactive heatmap
    p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
      ggplot2::geom_tile_interactive(ggplot2::aes(tooltip = sprintf("%.2f", value))) +
      ggplot2::scale_fill_gradient2(
        low = "#d73027",
        mid = "#ffffff",
        high = "#1a9850",
        midpoint = 0,
        limits = c(-1, 1)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.text = ggplot2::element_text(size = 8)
      ) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = "Correlation"
      )

    girafe(ggobj = p, width_svg = 10, height_svg = 8)
  })

  # Correlation insights
  output$insights_correlation_insights <- renderUI({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(div(class = "alert alert-warning", "Not enough questions for correlation analysis"))
    }

    # Calculate correlation matrix
    cor_matrix <- cor(data[, existing_cols], use = "pairwise.complete.obs")

    # Find strongest correlations
    cor_df <- reshape2::melt(cor_matrix)
    colnames(cor_df) <- c("Var1", "Var2", "correlation")

    # Shorten names
    cor_df$var1 <- labels[match(cor_df$Var1, existing_cols)]
    cor_df$var2 <- labels[match(cor_df$Var2, existing_cols)]

    # Remove self-correlations and duplicates
    cor_df <- cor_df %>%
      dplyr::filter(Var1 != Var2) %>%
      dplyr::mutate(
        pair = purrr::map2_chr(Var1, Var2, function(x, y) {
          paste0(sort(c(x, y)), collapse = "|||")
        })
      ) %>%
      dplyr::distinct(pair, .keep_all = TRUE) %>%
      dplyr::select(-pair)

    ui_correlation_insights(cor_df)
  })

  # Regression analysis plot
  output$insights_regression_plot <- renderPlot({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(NULL)
    }

    # Use first question as dependent variable
    dep_var <- existing_cols[1]
    indep_vars <- existing_cols[-1]

    # Prepare data
    reg_data <- data[, c(dep_var, indep_vars)]
    reg_data <- na.omit(reg_data)

    if (nrow(reg_data) < 10) {
      return(NULL)
    }

    # Run regression
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = "+"))
    model <- lm(as.formula(formula_str), data = reg_data)

    # Extract coefficients
    coefs <- coef(model)[-1]
    coefs <- coefs[order(abs(coefs), decreasing = TRUE)]

    coef_df <- data.frame(
      variable = names(coefs),
      coefficient = coefs,
      abs_coef = abs(coefs)
    )

    # Map to labels
    coef_df$label <- labels[match(coef_df$variable, existing_cols)]

    ggplot2::ggplot(coef_df, ggplot2::aes(
      x = reorder(label, abs_coef),
      y = coefficient,
      fill = coefficient
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient2(low = "#d73027", high = "#1a9850", midpoint = 0) +
      ggplot2::labs(
        x = "Predictor",
        y = "Coefficient",
        title = "Regression Coefficients"
      ) +
      get_viz_theme() +
      ggplot2::theme(legend.position = "none")
  })

  # Regression table
  output$insights_regression_table <- DT::renderDataTable({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(NULL)
    }

    # Use first question as dependent variable
    dep_var <- existing_cols[1]
    indep_vars <- existing_cols[-1]

    # Prepare data
    reg_data <- data[, c(dep_var, indep_vars)]
    reg_data <- na.omit(reg_data)

    if (nrow(reg_data) < 10) {
      return(NULL)
    }

    # Run regression
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = "+"))
    model <- lm(as.formula(formula_str), data = reg_data)

    # Extract summary
    summary_model <- summary(model)
    coefs <- summary_model$coefficients

    result_df <- data.frame(
      Variable = rownames(coefs)[-1],
      Coefficient = coefs[-1, 1],
      `Std. Error` = coefs[-1, 2],
      `t-value` = coefs[-1, 3],
      `p-value` = coefs[-1, 4]
    )

    # Map to labels
    labels <- get_category_labels(category)
    result_df$Variable <- labels[match(result_df$Variable, existing_cols)]

    DT::datatable(
      result_df,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("Coefficient", "Std. Error", "t-value"), digits = 3) %>%
      DT::formatSignif(columns = "p-value", digits = 3)
  })

  # Cluster analysis plot
  output$insights_cluster_plot <- renderPlot({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(NULL)
    }

    # Prepare data for clustering
    cluster_data <- data[, existing_cols]
    cluster_data <- na.omit(cluster_data)

    if (nrow(cluster_data) < 10) {
      return(NULL)
    }

    # Standardize data
    cluster_data_scaled <- scale(cluster_data)

    # Perform k-means clustering
    set.seed(123)
    n_clusters <- min(4, nrow(cluster_data) %/% 10)
    if (n_clusters < 2) n_clusters <- 2

    kmeans_result <- kmeans(cluster_data_scaled, centers = n_clusters)

    # Create cluster distribution data
    cluster_df <- data.frame(
      cluster = factor(kmeans_result$cluster),
      size = 1
    )

    cluster_counts <- cluster_df %>%
      dplyr::count(cluster) %>%
      dplyr::mutate(percentage = n / sum(n) * 100)

    colors <- RColorBrewer::brewer.pal(n_clusters, "Set2")

    ggplot2::ggplot(cluster_counts, ggplot2::aes(
      x = cluster,
      y = n,
      fill = cluster
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.1f%%)", n, percentage)), vjust = -0.3) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(
        x = "Cluster",
        y = "Number of Students",
        title = "Student Segments"
      ) +
      get_viz_theme() +
      ggplot2::theme(legend.position = "none")
  })

  # Cluster profiles
  output$insights_cluster_profiles <- renderUI({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(NULL)
    }

    # Prepare data for clustering
    cluster_data <- data[, existing_cols]
    cluster_data <- na.omit(cluster_data)

    if (nrow(cluster_data) < 10) {
      return(NULL)
    }

    # Standardize data
    cluster_data_scaled <- scale(cluster_data)

    # Perform k-means clustering
    set.seed(123)
    n_clusters <- min(4, nrow(cluster_data) %/% 10)
    if (n_clusters < 2) n_clusters <- 2

    kmeans_result <- kmeans(cluster_data_scaled, centers = n_clusters)

    # Calculate cluster profiles
    cluster_profiles <- data.frame(
      cluster_id = 1:n_clusters,
      percentage = round(table(kmeans_result$cluster) / length(kmeans_result$cluster) * 100, 1),
      description = c(
        "High Engagement",
        "Satisfied but Quiet",
        "Struggling",
        "Moderate"
      )[1:n_clusters]
    )

    ui_cluster_profiles(cluster_profiles)
  })

  # Cluster characteristics table
  output$insights_cluster_table <- DT::renderDataTable({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 2) {
      return(NULL)
    }

    # Prepare data for clustering
    cluster_data <- data[, existing_cols]
    cluster_data <- na.omit(cluster_data)

    if (nrow(cluster_data) < 10) {
      return(NULL)
    }

    # Standardize data
    cluster_data_scaled <- scale(cluster_data)

    # Perform k-means clustering
    set.seed(123)
    n_clusters <- min(4, nrow(cluster_data) %/% 10)
    if (n_clusters < 2) n_clusters <- 2

    kmeans_result <- kmeans(cluster_data_scaled, centers = n_clusters)

    # Calculate cluster means
    cluster_means <- data.frame(cluster = kmeans_result$cluster, cluster_data)
    cluster_means <- cluster_means %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarize_all(mean, na.rm = TRUE) %>%
      dplyr::ungroup()

    # Transpose for display
    cluster_means_t <- t(cluster_means[, -1])
    colnames(cluster_means_t) <- paste0("Cluster ", 1:n_clusters)

    DT::datatable(
      cluster_means_t,
      options = list(pageLength = 10),
      rownames = TRUE
    ) %>%
      DT::formatRound(columns = 1:n_clusters, digits = 2)
  })

  # Section comparison plot
  output$insights_section_comparison_plot <- renderPlot({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    labels <- get_category_labels(category)

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]
    section_col <- get_section_col()

    if (length(existing_cols) < 1 || !section_col %in% colnames(data)) {
      return(NULL)
    }

    # Calculate means by section
    section_means <- data %>%
      dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
      dplyr::group_by(.data[[section_col]]) %>%
      dplyr::summarize_at(existing_cols, mean, na.rm = TRUE) %>%
      dplyr::ungroup()

    if (nrow(section_means) == 0) {
      return(NULL)
    }

    # Reshape for plotting
    section_means_long <- reshape2::melt(section_means, id.vars = section_col)
    section_means_long$variable <- labels[match(section_means_long$variable, existing_cols)]

    ggplot2::ggplot(section_means_long, ggplot2::aes(
      x = .data[[section_col]],
      y = value,
      fill = .data[[section_col]]
    )) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::facet_wrap(~variable) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(
        x = "Section",
        y = "Mean Score",
        title = "Section Comparison by Question"
      ) +
      get_viz_theme() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::ylim(0, 5)
  })

  # ANOVA results
  output$insights_anova_results <- renderUI({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    section_col <- get_section_col()

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 1 || !section_col %in% colnames(data)) {
      return(NULL)
    }

    # Run ANOVA for first question
    dep_var <- existing_cols[1]

    anova_result <- aov(data[[dep_var]] ~ data[[section_col]], data = data)
    anova_summary <- summary(anova_result)[[1]]

    result <- list(
      f_statistic = anova_summary$`F value`[1],
      p_value = anova_summary$`Pr(>F)`[1]
    )

    ui_anova_results(result)
  })

  # Effect sizes table
  output$insights_effect_sizes <- DT::renderDataTable({
    data <- rv$current_data
    category <- selected_category()
    questions <- get_category_questions(category)
    section_col <- get_section_col()

    # Filter to existing columns
    existing_cols <- questions[questions %in% colnames(data)]

    if (length(existing_cols) < 1 || !section_col %in% colnames(data)) {
      return(NULL)
    }

    # Calculate effect sizes for each question
    effect_sizes <- lapply(existing_cols, function(q_col) {
      # Get data by section
      sections <- unique(data[[section_col]])
      sections <- sections[!is.na(sections) & sections != ""]

      if (length(sections) < 2) {
        return(NULL)
      }

      # Calculate pairwise Cohen's d
      section_data <- lapply(sections, function(s) {
        data[data[[section_col]] == s, ][[q_col]]
      })

      # Simple effect size: difference in means / pooled SD
      means <- sapply(section_data, mean, na.rm = TRUE)
      sds <- sapply(section_data, sd, na.rm = TRUE)
      pooled_sd <- sqrt(mean(sds^2, na.rm = TRUE))

      if (pooled_sd == 0) {
        effect_size <- 0
      } else {
        effect_size <- (max(means) - min(means)) / pooled_sd
      }

      data.frame(
        Question = q_col,
        `Effect Size (d)` = effect_size,
        Interpretation = dplyr::case_when(
          abs(effect_size) < 0.2 ~ "Negligible",
          abs(effect_size) < 0.5 ~ "Small",
          abs(effect_size) < 0.8 ~ "Medium",
          TRUE ~ "Large"
        )
      )
    })

    effect_sizes <- effect_sizes[!sapply(effect_sizes, is.null)]

    if (length(effect_sizes) == 0) {
      return(NULL)
    }

    do.call(rbind, effect_sizes) %>%
      DT::datatable(options = list(pageLength = 10)) %>%
      DT::formatRound(columns = "Effect Size (d)", digits = 3)
  })

  # Reliability plot
  output$insights_reliability_plot <- renderPlot({
    data <- rv$current_data

    # Calculate Cronbach's alpha for each category
    categories <- c("course_satisfaction", "learning_methods", "community_belonging")

    alphas <- sapply(categories, function(cat) {
      questions <- get_category_questions(cat)
      existing_cols <- questions[questions %in% colnames(data)]

      if (length(existing_cols) < 2) {
        return(NA)
      }

      # Calculate alpha
      subset_data <- data[, existing_cols]
      subset_data <- na.omit(subset_data)

      if (nrow(subset_data) < 10) {
        return(NA)
      }

      cronbach_alpha(subset_data)
    })

    alpha_df <- data.frame(
      category = sapply(categories, get_category_display_name),
      alpha = alphas
    ) %>%
      dplyr::filter(!is.na(alpha))

    colors <- c("#3498db", "#2ecc71", "#9b59b6")

    ggplot2::ggplot(alpha_df, ggplot2::aes(
      x = reorder(category, -alpha),
      y = alpha,
      fill = category
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", alpha)), vjust = -0.3) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        x = "Category",
        y = "Cronbach's Alpha",
        title = "Internal Consistency by Category"
      ) +
      get_viz_theme() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, 1)
  })

  # Reliability table
  output$insights_reliability_table <- DT::renderDataTable({
    data <- rv$current_data

    categories <- c("course_satisfaction", "learning_methods", "community_belonging")

    results <- lapply(categories, function(cat) {
      questions <- get_category_questions(cat)
      existing_cols <- questions[questions %in% colnames(data)]

      if (length(existing_cols) < 2) {
        return(NULL)
      }

      subset_data <- data[, existing_cols]
      subset_data <- na.omit(subset_data)

      if (nrow(subset_data) < 10) {
        return(NULL)
      }

      alpha <- cronbach_alpha(subset_data)

      data.frame(
        Category = get_category_display_name(cat),
        `Cronbach's Alpha` = alpha,
        Interpretation = dplyr::case_when(
          alpha >= 0.9 ~ "Excellent",
          alpha >= 0.8 ~ "Good",
          alpha >= 0.7 ~ "Acceptable",
          alpha >= 0.6 ~ "Questionable",
          alpha >= 0.5 ~ "Poor",
          TRUE ~ "Unacceptable"
        ),
        `N Items` = length(existing_cols),
        `N Respondents` = nrow(subset_data)
      )
    })

    results <- results[!sapply(results, is.null)]

    if (length(results) == 0) {
      return(NULL)
    }

    do.call(rbind, results) %>%
      DT::datatable(options = list(pageLength = 10)) %>%
      DT::formatRound(columns = "Cronbach's Alpha", digits = 3)
  })

  # Interaction effects
  output$insights_interactions <- renderUI({
    # Placeholder for interaction analysis
    div(
      class = "alert alert-info",
      icon("info-circle"),
      " Interaction analysis requires more complex modeling. This feature will be available in a future version."
    )
  })
}

#' Calculate Cronbach's alpha
#'
#' Computes Cronbach's alpha for internal consistency
#'
#' @param data Data frame with Likert responses
#' @return Cronbach's alpha value
#' @export
cronbach_alpha <- function(data) {
  n_items <- ncol(data)
  n_respondents <- nrow(data)

  if (n_items < 2 || n_respondents < 2) {
    return(NA)
  }

  # Calculate item variances
  item_variances <- sapply(data, var, na.rm = TRUE)

  # Calculate total score variance
  total_scores <- rowSums(data, na.rm = TRUE)
  total_variance <- var(total_scores)

  if (total_variance == 0) {
    return(NA)
  }

  # Calculate alpha
  alpha <- (n_items / (n_items - 1)) * (1 - sum(item_variances) / total_variance)

  return(alpha)
}

#' Calculate Cohen's d effect size
#'
#' Computes Cohen's d for comparing two groups
#'
#' @param group1 Numeric vector for group 1
#' @param group2 Numeric vector for group 2
#' @return Cohen's d value
#' @export
calculate_cohens_d <- function(group1, group2) {
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
