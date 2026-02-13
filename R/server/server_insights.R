# Insights Generation Module
# Functions for generating automated insights from survey data

#' Generate Overview Insights
#'
#' Generates key insights for the Overview tab including response patterns,
# demographic distributions, and overall satisfaction metrics.
#'
#' @param df A data frame containing survey responses
#' @return A list of insight objects with type and content
#' @export
generate_overview_insights <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      list(type = "Info", content = "No data available for insights.")
    ))
  }
  
  insights <- list()
  
  # Total responses insight
  total_responses <- nrow(df)
  insights <- c(insights, list(list(
    type = "Key Finding",
    content = paste0("The survey received ", total_responses, " total responses across all sections.")
  )))
  
  # Section distribution insight
  if ("section" %in% names(df)) {
    section_counts <- df %>%
      filter(!is.na(section)) %>%
      count(section) %>%
      arrange(desc(n))
    
    if (nrow(section_counts) > 0) {
      top_section <- section_counts$section[1]
      top_count <- section_counts$n[1]
      top_pct <- round(top_count / total_responses * 100, 1)
      
      insights <- c(insights, list(list(
        type = "Key Finding",
        content = paste0("Section ", top_section, " had the highest response rate with ", 
                         top_count, " responses (", top_pct, "%).")
      )))
    }
  }
  
  # Experience distribution insight
  if ("prior_experience" %in% names(df)) {
    exp_counts <- df %>%
      filter(!is.na(prior_experience)) %>%
      count(prior_experience)
    
    if (nrow(exp_counts) > 0) {
      most_common_exp <- exp_counts$prior_experience[which.max(exp_counts$n)]
      exp_pct <- round(max(exp_counts$n) / sum(exp_counts$n) * 100, 1)
      
      exp_label <- case_when(
        most_common_exp == "No experience at all" ~ "no prior experience",
        most_common_exp == "Took programming course before (either in school, or online tutorials)" ~ "some programming experience",
        most_common_exp == "Highly experienced (comfortable writing own programs)" ~ "high programming experience",
        TRUE ~ "mixed experience levels"
      )
      
      insights <- c(insights, list(list(
        type = "Notable Pattern",
        content = paste0(exp_pct, "% of respondents reported ", exp_label, ".")
      )))
    }
  }
  
  # Learning preference insight
  if ("learning_preference" %in% names(df)) {
    pref_counts <- df %>%
      filter(!is.na(learning_preference)) %>%
      count(learning_preference)
    
    if (nrow(pref_counts) > 0) {
      top_pref <- pref_counts$learning_preference[which.max(pref_counts$n)]
      pref_pct <- round(max(pref_counts$n) / sum(pref_counts$n) * 100, 1)
      
      insights <- c(insights, list(list(
        type = "Notable Pattern",
        content = paste0(pref_pct, "% of students prefer ", tolower(top_pref), " learning.")
      )))
    }
  }
  
  # Course agreement insight
  agreement_cols <- c(
    "how_much_do_you_agree_with_the_statement_1",
    "how_much_do_you_agree_with_the_statement_2",
    "how_much_do_you_agree_with_the_statement_3",
    "how_much_do_you_agree_with_the_statement_4",
    "how_much_do_you_agree_with_the_statement_5",
    "how_much_do_you_agree_with_the_statement_6"
  )
  
  existing_agreement <- intersect(agreement_cols, names(df))
  if (length(existing_agreement) > 0) {
    # Calculate average agreement
    agreement_values <- unlist(df[, existing_agreement, drop = FALSE])
    agreement_values <- agreement_values[!is.na(agreement_values)]
    
    if (length(agreement_values) > 0) {
      avg_agreement <- round(mean(agreement_values), 2)
      agree_pct <- round(sum(agreement_values >= 4) / length(agreement_values) * 100, 1)
      
      sentiment <- case_when(
        avg_agreement >= 4 ~ "positive",
        avg_agreement >= 3 ~ "neutral",
        TRUE ~ "negative"
      )
      
      insights <- c(insights, list(list(
        type = "Key Finding",
        content = paste0("Overall course agreement is ", sentiment, " with an average score of ", 
                         avg_agreement, "/5. ", agree_pct, "% of responses were positive.")
      )))
    }
  }
  
  return(insights)
}

#' Generate Course Content Insights
#'
#' Generates insights for the Course Content tab including agreement patterns,
# statement rankings, and section comparisons.
#'
#' @param df A data frame containing survey responses
#' @return A list of insight objects with type and content
#' @export
generate_course_content_insights <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      list(type = "Info", content = "No data available for insights.")
    ))
  }
  
  insights <- list()
  
  # Agreement statement columns
  agreement_cols <- c(
    "how_much_do_you_agree_with_the_statement_1",
    "how_much_do_you_agree_with_the_statement_2",
    "how_much_do_you_agree_with_the_statement_3",
    "how_much_do_you_agree_with_the_statement_4",
    "how_much_do_you_agree_with_the_statement_5",
    "how_much_do_you_agree_with_the_statement_6"
  )
  
  existing_agreement <- intersect(agreement_cols, names(df))
  
  if (length(existing_agreement) > 0) {
    # Calculate average for each statement
    statement_avgs <- sapply(existing_agreement, function(col) {
      values <- df[[col]]
      values <- values[!is.na(values)]
      if (length(values) > 0) mean(values) else NA
    })
    
    statement_avgs <- statement_avgs[!is.na(statement_avgs)]
    
    if (length(statement_avgs) > 0) {
      # Find highest and lowest rated statements
      highest_idx <- which.max(statement_avgs)
      lowest_idx <- which.min(statement_avgs)
      
      highest_col <- names(statement_avgs)[highest_idx]
      lowest_col <- names(statement_avgs)[lowest_idx]
      
      highest_score <- round(statement_avgs[highest_idx], 2)
      lowest_score <- round(statement_avgs[lowest_idx], 2)
      
      insights <- c(insights, list(list(
        type = "Key Finding",
        content = paste0("The highest rated aspect was '", 
                         substr(get_column_display_name(highest_col), 1, 50), 
                         "' with an average of ", highest_score, "/5.")
      )))
      
      insights <- c(insights, list(list(
        type = "Recommendation",
        content = paste0("Consider reviewing '", 
                         substr(get_column_display_name(lowest_col), 1, 50), 
                         "' which had the lowest rating (", lowest_score, "/5).")
      )))
    }
    
    # Check for consensus
    consensus_scores <- sapply(existing_agreement, function(col) {
      values <- df[[col]]
      values <- values[!is.na(values)]
      if (length(values) > 0) sd(values) else NA
    })
    
    consensus_scores <- consensus_scores[!is.na(consensus_scores)]
    
    if (length(consensus_scores) > 0) {
      high_consensus_cols <- names(consensus_scores)[consensus_scores < 1.0]
      low_consensus_cols <- names(consensus_scores)[consensus_scores > 1.5]
      
      if (length(high_consensus_cols) > 0) {
        insights <- c(insights, list(list(
          type = "Notable Pattern",
          content = paste0("There is strong consensus on ", length(high_consensus_cols), 
                           " course aspects, indicating clear student opinions.")
        )))
      }
      
      if (length(low_consensus_cols) > 0) {
        insights <- c(insights, list(list(
          type = "Notable Pattern",
          content = paste0("There is significant disagreement on ", length(low_consensus_cols), 
                           " course aspects, suggesting diverse student experiences.")
        )))
      }
    }
  }
  
  # Section comparison insight
  if ("section" %in% names(df) && length(existing_agreement) > 0) {
    sections <- unique(df$section)
    sections <- sections[!is.na(sections)]
    
    if (length(sections) >= 2) {
      # Calculate average agreement per section
      section_avgs <- sapply(sections, function(sec) {
        sec_data <- df[df$section == sec, ]
        values <- unlist(sec_data[, existing_agreement, drop = FALSE])
        values <- values[!is.na(values)]
        if (length(values) > 0) mean(values) else NA
      })
      
      section_avgs <- section_avgs[!is.na(section_avgs)]
      
      if (length(section_avgs) > 1) {
        max_diff <- max(section_avgs) - min(section_avgs)
        
        if (max_diff > 0.5) {
          insights <- c(insights, list(list(
            type = "Notable Pattern",
            content = paste0("There is a ", round(max_diff, 2), 
                             " point difference in average agreement between sections, suggesting varying experiences.")
          )))
        }
      }
    }
  }
  
  return(insights)
}

#' Generate Learning Elements Insights
#'
#' Generates insights for the Learning Elements tab including element rankings,
# correlations, and experience-based patterns.
#'
#' @param df A data frame containing survey responses
#' @return A list of insight objects with type and content
#' @export
generate_learning_elements_insights <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      list(type = "Info", content = "No data available for insights.")
    ))
  }
  
  insights <- list()
  
  # Learning element columns
  learning_cols <- c(
    "how_much_do_the_following_elements_contribute_to_your_learning_1",
    "how_much_do_the_following_elements_contribute_to_your_learning_2",
    "how_much_do_the_following_elements_contribute_to_your_learning_3",
    "how_much_do_the_following_elements_contribute_to_your_learning_4",
    "how_much_do_the_following_elements_contribute_to_your_learning_5",
    "how_much_do_the_following_elements_contribute_to_your_learning_6",
    "how_much_do_the_following_elements_contribute_to_your_learning_7",
    "how_much_do_the_following_elements_contribute_to_your_learning_8",
    "how_much_do_the_following_elements_contribute_to_your_learning_9",
    "how_much_do_the_following_elements_contribute_to_your_learning_10",
    "how_much_do_the_following_elements_contribute_to_your_learning_11"
  )
  
  existing_learning <- intersect(learning_cols, names(df))
  
  if (length(existing_learning) > 0) {
    # Calculate average for each element
    element_avgs <- sapply(existing_learning, function(col) {
      values <- df[[col]]
      values <- values[!is.na(values)]
      if (length(values) > 0) mean(values) else NA
    })
    
    element_avgs <- element_avgs[!is.na(element_avgs)]
    
    if (length(element_avgs) > 0) {
      # Find top 3 elements
      top3_idx <- order(element_avgs, decreasing = TRUE)[1:min(3, length(element_avgs))]
      
      top_elements <- sapply(top3_idx, function(i) {
        col <- names(element_avgs)[i]
        substr(get_column_display_name(col), 1, 40)
      })
      
      insights <- c(insights, list(list(
        type = "Key Finding",
        content = paste0("Top learning contributors: ", 
                         paste(top_elements, collapse = ", "), ".")
      )))
      
      # Find bottom 3 elements
      bottom3_idx <- order(element_avgs)[1:min(3, length(element_avgs))]
      
      bottom_elements <- sapply(bottom3_idx, function(i) {
        col <- names(element_avgs)[i]
        substr(get_column_display_name(col), 1, 40)
      })
      
      insights <- c(insights, list(list(
        type = "Recommendation",
        content = paste0("Consider enhancing: ", 
                         paste(bottom_elements, collapse = ", "), ".")
      )))
    }
    
    # Check for correlation patterns
    if (length(existing_learning) >= 3) {
      cor_matrix <- calculate_correlation_matrix(df, existing_learning)
      
      if (!is.null(cor_matrix)) {
        # Find highest correlation (excluding diagonal)
        cor_values <- cor_matrix[upper.tri(cor_matrix)]
        max_cor <- max(cor_values, na.rm = TRUE)
        
        if (!is.na(max_cor) && max_cor > 0.6) {
          insights <- c(insights, list(list(
            type = "Notable Pattern",
            content = paste0("Some learning elements are strongly correlated (r = ", 
                             round(max_cor, 2), "), suggesting students perceive them as related.")
          )))
        }
      }
    }
  }
  
  # Experience-based insight
  if ("prior_experience" %in% names(df) && length(existing_learning) > 0) {
    exp_levels <- c(
      "No experience at all",
      "Took programming course before (either in school, or online tutorials)",
      "Highly experienced (comfortable writing own programs)"
    )
    
    exp_avgs <- sapply(exp_levels, function(exp) {
      exp_data <- df[df$prior_experience == exp, ]
      values <- unlist(exp_data[, existing_learning, drop = FALSE])
      values <- values[!is.na(values)]
      if (length(values) > 0) mean(values) else NA
    })
    
    exp_avgs <- exp_avgs[!is.na(exp_avgs)]
    
    if (length(exp_avgs) > 1) {
      max_diff <- max(exp_avgs) - min(exp_avgs)
      
      if (max_diff > 0.5) {
        insights <- c(insights, list(list(
          type = "Notable Pattern",
          content = paste0("Learning element ratings vary by experience level (", 
                           round(max_diff, 2), " point difference), suggesting different needs.")
        )))
      }
    }
  }
  
  return(insights)
}

#' Generate Community Insights
#'
#' Generates insights for the Community & Belonging tab including belonging
# scores, Discord usage patterns, and social challenges.
#'
#' @param df A data frame containing survey responses
#' @return A list of insight objects with type and content
#' @export
generate_community_insights <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      list(type = "Info", content = "No data available for insights.")
    ))
  }
  
  insights <- list()
  
  # Belonging statement columns
  belonging_cols <- c(
    "how_much_do_you_agree_with_the_following_statements_1",
    "how_much_do_you_agree_with_the_following_statements_2",
    "how_much_do_you_agree_with_the_following_statements_3",
    "how_much_do_you_agree_with_the_following_statements_4",
    "how_much_do_you_agree_with_the_following_statements_5"
  )
  
  existing_belonging <- intersect(belonging_cols, names(df))
  
  if (length(existing_belonging) > 0) {
    # Calculate overall belonging score
    belonging_values <- unlist(df[, existing_belonging, drop = FALSE])
    belonging_values <- belonging_values[!is.na(belonging_values)]
    
    if (length(belonging_values) > 0) {
      avg_belonging <- round(mean(belonging_values), 2)
      positive_pct <- round(sum(belonging_values >= 4) / length(belonging_values) * 100, 1)
      
      belonging_level <- case_when(
        avg_belonging >= 4 ~ "high",
        avg_belonging >= 3 ~ "moderate",
        TRUE ~ "low"
      )
      
      insights <- c(insights, list(list(
        type = "Key Finding",
        content = paste0("Overall sense of belonging is ", belonging_level, 
                         " (average ", avg_belonging, "/5). ", positive_pct, 
                         "% of students report positive belonging.")
      )))
    }
    
    # Find lowest rated belonging statement
    statement_avgs <- sapply(existing_belonging, function(col) {
      values <- df[[col]]
      values <- values[!is.na(values)]
      if (length(values) > 0) mean(values) else NA
    })
    
    statement_avgs <- statement_avgs[!is.na(statement_avgs)]
    
    if (length(statement_avgs) > 0) {
      lowest_idx <- which.min(statement_avgs)
      lowest_col <- names(statement_avgs)[lowest_idx]
      lowest_score <- round(statement_avgs[lowest_idx], 2)
      
      if (lowest_score < 3.5) {
        insights <- c(insights, list(list(
          type = "Recommendation",
          content = paste0("Consider addressing '", 
                           substr(get_column_display_name(lowest_col), 1, 50), 
                           "' which had the lowest belonging score (", lowest_score, "/5).")
        )))
      }
    }
  }
  
  # Discord usage insight
  discord_cols <- grep("^discord_", names(df), value = TRUE)
  discord_cols <- setdiff(discord_cols, "discord_custom_response")
  
  if (length(discord_cols) > 0) {
    discord_usage <- rowSums(df[, discord_cols, drop = FALSE], na.rm = TRUE)
    discord_usage <- discord_usage[!is.na(discord_usage)]
    
    if (length(discord_usage) > 0) {
      active_users <- sum(discord_usage > 0)
      total_users <- length(discord_usage)
      active_pct <- round(active_users / total_users * 100, 1)
      
      insights <- c(insights, list(list(
        type = "Key Finding",
        content = paste0(active_pct, "% of students use Discord for class communication.")
      )))
      
      # Most popular Discord feature
      feature_counts <- sapply(discord_cols, function(col) {
        sum(df[[col]] == 1, na.rm = TRUE)
      })
      
      top_feature_idx <- which.max(feature_counts)
      top_feature <- names(feature_counts)[top_feature_idx]
      top_feature <- gsub("^discord_", "", top_feature)
      top_feature <- gsub("_", " ", top_feature)
      
      insights <- c(insights, list(list(
        type = "Notable Pattern",
        content = paste0("The most popular Discord feature is '", top_feature, "'.")
      )))
    }
  }
  
  return(insights)
}
