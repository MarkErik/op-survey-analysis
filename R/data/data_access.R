# Data Access Module
# Functions for accessing processed survey data

#' Resolve Column Alias
#'
#' Resolves a numbered alias to the actual column name in the data.
#'
#' @param alias A column alias (e.g., "how_much_do_you_agree_with_the_statement_1")
#' @return The actual column name, or the alias if not found
#' @export
resolve_column_alias <- function(alias) {
  if (exists("column_mappings") && alias %in% names(column_mappings$aliases)) {
    return(column_mappings$aliases[[alias]])
  }
  return(alias)
}

#' Get Column Display Name
#'
#' Retrieves the original column name (question text) for display in UI/charts.
#'
#' @param normalized_name A normalized column name or alias
#' @return The original column name (question text), or the normalized name if not found
#' @export
get_column_display_name <- function(normalized_name) {
  # Resolve alias if needed
  actual_name <- resolve_column_alias(normalized_name)
  idx <- which(column_mappings$normalized == actual_name)
  if (length(idx) == 0) {
    return(normalized_name)
  }
  return(column_mappings$original[idx[1]])
}

#' Get Display Prefix
#'
#' Retrieves the question stem (prefix) for a column.
#'
#' @param normalized_name A normalized column name or alias
#' @return The question prefix, or empty string if not found
#' @export
get_display_prefix <- function(normalized_name) {
  actual_name <- resolve_column_alias(normalized_name)
  if (exists("column_mappings") && actual_name %in% names(column_mappings$display$prefix)) {
    return(column_mappings$display$prefix[actual_name])
  }
  return("")
}

#' Get Display Core
#'
#' Retrieves the core content for a column (the specific question content).
#'
#' @param normalized_name A normalized column name or alias
#' @return The core content, or the column name if not found
#' @export
get_display_core <- function(normalized_name) {
  actual_name <- resolve_column_alias(normalized_name)
  if (exists("column_mappings") && actual_name %in% names(column_mappings$display$core)) {
    return(column_mappings$display$core[actual_name])
  }
  return(normalized_name)
}

#' Get Display Short
#'
#' Retrieves a compact label for a column suitable for charts and tight spaces.
#'
#' @param normalized_name A normalized column name or alias
#' @return A short label, or the column name if not found
#' @export
get_display_short <- function(normalized_name) {
  actual_name <- resolve_column_alias(normalized_name)
  if (exists("column_mappings") && actual_name %in% names(column_mappings$display$short)) {
    return(column_mappings$display$short[actual_name])
  }
  return(normalized_name)
}

#' Get Display Full
#'
#' Retrieves a cleaned full question text for a column.
#'
#' @param normalized_name A normalized column name or alias
#' @return The cleaned full question text
#' @export
get_display_full <- function(normalized_name) {
  actual_name <- resolve_column_alias(normalized_name)
  prefix <- get_display_prefix(actual_name)
  core <- get_display_core(actual_name)
  suffix <- if (exists("column_mappings") && actual_name %in% names(column_mappings$display$suffix)) {
    column_mappings$display$suffix[actual_name]
  } else {
    ""
  }
  
  # Build full display text
  if (!is.null(prefix) && prefix != "") {
    full <- paste0(prefix, ": ", core)
  } else {
    full <- core
  }
  
  if (!is.null(suffix) && suffix != "") {
    full <- paste0(full, " ", suffix)
  }
  
  return(full)
}

#' Get All Column Display Names
#'
#' Returns a named vector mapping normalized column names to their display names.
#'
#' @return A named vector with normalized names as names and original names as values
#' @export
get_all_column_display_names <- function() {
  setNames(column_mappings$original, column_mappings$normalized)
}

#' Get Column Mappings
#'
#' Returns the global column mappings list.
#'
#' @return A list with 'original', 'normalized', 'aliases', and 'display' components
#' @export
get_column_mappings <- function() {
  return(column_mappings)
}

#' Get Survey Data
#'
#' Returns the full processed survey data frame.
#'
#' @return The processed survey data frame, or NULL if not available
#' @export
get_survey_data <- function() {
  return(survey_data)
}

#' Get Data by Section
#'
#' Returns data filtered by course number and/or time slot.
#'
#' @param course_number Optional course number (e.g., "231", "217")
#' @param section_time Optional time slot (e.g., "1pm", "11am")
#' @return A filtered data frame, or NULL if survey_data is not available
#' @export
get_data_by_section <- function(course_number = NULL, section_time = NULL) {
  if (is.null(survey_data)) {
    return(NULL)
  }
  
  df <- survey_data
  
  if (!is.null(course_number)) {
    df <- df %>% filter(course_number == course_number)
  }
  
  if (!is.null(section_time)) {
    df <- df %>% filter(section_time == section_time)
  }
  
  return(df)
}

#' Get Likert Data
#'
#' Returns only Likert scale columns for a specific category.
#'
#' @param category One of "course", "learning", or "community"
#' @return A data frame with participant_id and the requested Likert columns
#' @export
get_likert_data <- function(category = NULL) {
  if (is.null(survey_data)) {
    return(NULL)
  }
  
  # Define column groups
  likert_columns <- list(
    course = c(
      "how_much_do_you_agree_with_the_statement_1",
      "how_much_do_you_agree_with_the_statement_2",
      "how_much_do_you_agree_with_the_statement_3",
      "how_much_do_you_agree_with_the_statement_4",
      "how_much_do_you_agree_with_the_statement_5",
      "how_much_do_you_agree_with_the_statement_6"
    ),
    learning = c(
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
    ),
    community = c(
      "how_much_do_you_agree_with_the_following_statements_1",
      "how_much_do_you_agree_with_the_following_statements_2",
      "how_much_do_you_agree_with_the_following_statements_3",
      "how_much_do_you_agree_with_the_following_statements_4",
      "how_much_do_you_agree_with_the_following_statements_5"
    )
  )
  
  if (is.null(category)) {
    # Return all Likert columns
    cols <- unlist(likert_columns)
  } else if (category %in% names(likert_columns)) {
    cols <- likert_columns[[category]]
  } else {
    return(NULL)
  }
  
  # Resolve aliases to actual column names
  actual_cols <- sapply(cols, resolve_column_alias)
  
  # Filter to only existing columns
  existing_cols <- intersect(actual_cols, names(survey_data))
  
  if (length(existing_cols) == 0) {
    return(NULL)
  }
  
  df <- survey_data %>%
    select(participant_id, all_of(existing_cols))
  
  return(df)
}

#' Get Free Text Responses
#'
#' Returns non-empty responses for a specific free text question.
#'
#' @param question_column The normalized column name of the free text question
#' @return A data frame with participant_id and the question responses, or NULL if not found
#' @export
get_free_text_responses <- function(question_column) {
  if (is.null(survey_data)) {
    return(NULL)
  }
  
  if (!question_column %in% names(survey_data)) {
    return(NULL)
  }
  
  df <- survey_data %>%
    filter(!is.na(!!sym(question_column)) & !!sym(question_column) != "") %>%
    select(participant_id, all_of(question_column))
  
  return(df)
}

#' Get Discord Statistics
#'
#' Returns summary statistics for Discord usage.
#'
#' @return A data frame with counts for each Discord option, or NULL if not available
#' @export
get_discord_stats <- function() {
  if (is.null(survey_data)) {
    return(NULL)
  }
  
  discord_cols <- grep("^discord_", names(survey_data), value = TRUE)
  discord_cols <- setdiff(discord_cols, "discord_custom_response")
  
  if (length(discord_cols) == 0) {
    return(NULL)
  }
  
  stats <- survey_data %>%
    select(all_of(discord_cols)) %>%
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "option",
      values_to = "count"
    ) %>%
    mutate(
      option = gsub("^discord_", "", option),
      option = gsub("_", " ", option)
    )
  
  return(stats)
}

#' Get Statistics
#'
#' Returns general statistics for the home page.
#'
#' @return A list with total_responses, unique_sections, and other summary stats
#' @export
get_statistics <- function() {
  if (is.null(survey_data)) {
    return(list(
      total_responses = 0,
      unique_sections = 0,
      unique_courses = 0
    ))
  }
  
  return(list(
    total_responses = nrow(survey_data),
    unique_sections = length(unique(survey_data$section)),
    unique_courses = length(unique(survey_data$course_number))
  ))
}
