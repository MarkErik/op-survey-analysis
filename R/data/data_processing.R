# Data Processing Module
# Functions for transforming and processing survey data

#' Normalize Column Names
#'
#' Converts column names to lowercase snake_case for programmatic use.
#' Stores original names as an attribute temporarily for building mappings.
#'
#' @param df A data frame
#' @return A data frame with normalized column names
#' @export
normalize_column_names <- function(df) {
  original_names <- names(df)
  names(df) <- tolower(names(df))
  names(df) <- gsub("[^a-z0-9_]", "_", names(df))
  names(df) <- gsub("_+", "_", names(df))
  names(df) <- gsub("^_|_$", "", names(df))
  attr(df, "original_column_names") <- original_names
  return(df)
}

#' Build Column Mappings
#'
#' Creates a global mapping between normalized column names and original
#' question text for UI display. Also creates numbered aliases for Likert
#' columns and parses display name components. This function has a side
#' effect of creating a global `column_mappings` variable.
#'
#' @param df A data frame with normalized column names
#' @return The data frame with the temporary attribute removed
#' @export
build_column_mappings <- function(df) {
  original_names <- attr(df, "original_column_names")
  if (is.null(original_names)) {
    original_names <- names(df)
  }
  
  normalized_names <- names(df)
  
  # Create aliases for Likert columns
  aliases <- c()
  alias_to_column <- c()
  
  # Course agreement statements
  course_agreement_cols <- normalized_names[grepl("^how_much_do_you_agree_with_the_statement_", normalized_names) &
                                           !grepl("^how_much_do_you_agree_with_the_following_statements_", normalized_names)]
  for (i in seq_along(course_agreement_cols)) {
    alias <- paste0("how_much_do_you_agree_with_the_statement_", i)
    aliases <- c(aliases, alias)
    alias_to_column <- c(alias_to_column, course_agreement_cols[i])
  }
  
  # Learning elements
  learning_cols <- normalized_names[grepl("^how_much_do_the_following_elements_contribute_to_your_learning_", normalized_names)]
  for (i in seq_along(learning_cols)) {
    alias <- paste0("how_much_do_the_following_elements_contribute_to_your_learning_", i)
    aliases <- c(aliases, alias)
    alias_to_column <- c(alias_to_column, learning_cols[i])
  }
  
  # Community statements
  community_cols <- normalized_names[grepl("^how_much_do_you_agree_with_the_following_statements_", normalized_names)]
  for (i in seq_along(community_cols)) {
    alias <- paste0("how_much_do_you_agree_with_the_following_statements_", i)
    aliases <- c(aliases, alias)
    alias_to_column <- c(alias_to_column, community_cols[i])
  }
  
  names(alias_to_column) <- aliases
  
  # Parse display name components for each column
  display_prefix <- character(length(normalized_names))
  display_core <- character(length(normalized_names))
  display_suffix <- character(length(normalized_names))
  display_short <- character(length(normalized_names))
  names(display_prefix) <- normalized_names
  names(display_core) <- normalized_names
  names(display_suffix) <- normalized_names
  names(display_short) <- normalized_names
  
  for (i in seq_along(normalized_names)) {
    orig <- original_names[i]
    
    # Extract prefix (question stem before brackets)
    if (grepl("\\[", orig)) {
      prefix <- trimws(gsub("\\[.*", "", orig))
      # Remove trailing punctuation
      prefix <- gsub("[?!.]$", "", prefix)
      display_prefix[i] <- prefix
      
      # Extract core (content within brackets)
      core <- gsub(".*\\[", "", orig)
      core <- gsub("\\].*", "", core)
      display_core[i] <- core
      
      # Extract suffix (anything after brackets)
      if (grepl("\\]", orig)) {
        suffix <- trimws(gsub(".*\\]", "", orig))
        display_suffix[i] <- suffix
      }
      
      # Create short label (first few words of core, capitalized)
      core_words <- strsplit(core, "\\s+")[[1]]
      if (length(core_words) > 0) {
        short_words <- core_words[1:min(3, length(core_words))]
        display_short[i] <- paste(toupper(substr(short_words, 1, 1)),
                                   substr(short_words, 2, nchar(short_words)),
                                   sep = "", collapse = " ")
      } else {
        display_short[i] <- core
      }
    } else {
      # No brackets, use full text as core
      display_prefix[i] <- ""
      display_core[i] <- orig
      display_suffix[i] <- ""
      # Create short from first few words
      words <- strsplit(orig, "\\s+")[[1]]
      if (length(words) > 0) {
        short_words <- words[1:min(3, length(words))]
        display_short[i] <- paste(toupper(substr(short_words, 1, 1)),
                                   substr(short_words, 2, nchar(short_words)),
                                   sep = "", collapse = " ")
      } else {
        display_short[i] <- orig
      }
    }
  }
  
  column_mappings <<- list(
    original = original_names,
    normalized = normalized_names,
    aliases = alias_to_column,
    display = list(
      prefix = display_prefix,
      core = display_core,
      suffix = display_suffix,
      short = display_short
    )
  )
  
  # Remove the temporary attribute
  attr(df, "original_column_names") <- NULL
  
  return(df)
}

#' Generate Participant IDs
#'
#' Creates synthetic participant IDs using timestamp + section + sequence.
#' Format: YYYYMMDDHHMMSS_SECTION_#### where #### is a zero-padded sequence number.
#'
#' @param df A data frame with timestamp and section columns
#' @return A data frame with participant_id column added as the first column
#' @export
generate_participant_ids <- function(df) {
  df <- df %>%
    arrange(timestamp, section) %>%
    mutate(
      section_seq = row_number(),
      participant_id = paste0(
        format(as.POSIXct(timestamp, format = "%Y/%m/%d %I:%M:%S %p"), "%Y%m%d%H%M%S"),
        "_",
        ifelse(is.na(section), "UNK", gsub("[^0-9a-zA-Z]", "", section)),
        "_",
        sprintf("%04d", section_seq)
      )
    ) %>%
    select(participant_id, everything(), -section_seq)
  return(df)
}

#' Parse Section Identifiers
#'
#' Splits the section column into course_number and section_time components.
#'
#' @param df A data frame with a section column
#' @return A data frame with course_number and section_time columns added
#' @export
parse_section_identifiers <- function(df) {
  df <- df %>%
    mutate(
      course_number = ifelse(
        grepl(" - ", section),
        trimws(gsub(" - .*", "", section)),
        NA_character_
      ),
      section_time = ifelse(
        grepl(" - ", section),
        trimws(gsub(".* - ", "", section)),
        NA_character_
      )
    )
  return(df)
}

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

#' Normalize Likert Scales
#'
#' Strips non-numeric characters from Likert responses to extract numeric rating (1-5).
#' Accepts either actual column names or numbered aliases.
#'
#' @param df A data frame
#' @param likert_columns Character vector of column names or aliases containing Likert responses
#' @return A data frame with Likert columns converted to integers
#' @export
normalize_likert_scales <- function(df, likert_columns) {
  # Resolve any aliases to actual column names
  actual_columns <- sapply(likert_columns, resolve_column_alias)
  
  # Filter to only existing columns
  existing_cols <- intersect(actual_columns, names(df))
  
  if (length(existing_cols) == 0) {
    return(df)
  }
  
  df <- df %>%
    mutate(across(
      all_of(existing_cols),
      ~ as.integer(gsub("[^0-9]", "", .x))
    ))
  return(df)
}

#' Parse Discord Responses
#'
#' Parses semicolon-separated Discord responses into binary columns for each option.
#'
#' @param df A data frame with a discord response column
#' @return A data frame with binary columns for each Discord option
#' @export
parse_discord_responses <- function(df) {
  discord_options <- c(
    "i have joined the class discord",
    "i am active in the class discord",
    "it is really useful for me for learning",
    "it is the main way that i connect with other students in this class",
    "i like that the class discord exists",
    "i don't like the amount of notifications",
    "i'm not sure what its purpose is",
    "i do not use the class discord at all",
    "i never joined it",
    "i did not join"
  )
  
  discord_col <- "about_the_class_discord_select_all_that_apply"
  
  if (!discord_col %in% names(df)) {
    return(df)
  }
  
  # Create binary columns for each option
  for (option in discord_options) {
    col_name <- paste0("discord_", gsub(" ", "_", option))
    df[[col_name]] <- ifelse(
      grepl(option, df[[discord_col]], ignore.case = TRUE),
      1L,
      0L
    )
  }
  
  # Add column for custom responses
  df$discord_custom_response <- ifelse(
    grepl(";", df[[discord_col]]) &
      !any(sapply(discord_options, function(opt) grepl(opt, df[[discord_col]], ignore.case = TRUE))),
    df[[discord_col]],
    NA_character_
  )
  
  return(df)
}

#' Clean Free Text
#'
#' Cleans free text columns by removing extra whitespace, normalizing line breaks,
#' and handling special characters.
#'
#' @param df A data frame
#' @param text_columns Character vector of column names containing free text
#' @return A data frame with cleaned text columns
#' @export
clean_free_text <- function(df, text_columns) {
  # Filter to only existing columns
  existing_cols <- intersect(text_columns, names(df))
  
  if (length(existing_cols) == 0) {
    return(df)
  }
  
  df <- df %>%
    mutate(across(
      all_of(existing_cols),
      ~ {
        result <- .x
        result[!is.na(result)] <- str_trim(result[!is.na(result)])
        result[!is.na(result)] <- gsub("\\r\\n|\\r|\\n", " ", result[!is.na(result)])
        result[!is.na(result)] <- gsub("\\s+", " ", result[!is.na(result)])
        result[!is.na(result)] <- str_trim(result[!is.na(result)])
        result
      }
    ))
  return(df)
}

#' Process Survey Data
#'
#' Orchestrates all processing steps to transform raw survey data into
#' a clean, normalized format ready for analysis and visualization.
#'
#' @param raw_df A raw data frame loaded from CSV
#' @return A processed data frame, or NULL if input is NULL
#' @export
process_survey_data <- function(raw_df) {
  if (is.null(raw_df)) return(NULL)
  
  df <- raw_df %>%
    normalize_column_names() %>%
    build_column_mappings() %>%
    rename(section = what_section_are_you_in) %>%
    generate_participant_ids() %>%
    parse_section_identifiers()
  
  # Define column groups using numbered aliases
  likert_columns <- c(
    # Course agreement (columns 8-13)
    "how_much_do_you_agree_with_the_statement_1",
    "how_much_do_you_agree_with_the_statement_2",
    "how_much_do_you_agree_with_the_statement_3",
    "how_much_do_you_agree_with_the_statement_4",
    "how_much_do_you_agree_with_the_statement_5",
    "how_much_do_you_agree_with_the_statement_6",
    # Learning elements (columns 14-24)
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
    "how_much_do_the_following_elements_contribute_to_your_learning_11",
    # Community statements (columns 28-32)
    "how_much_do_you_agree_with_the_following_statements_1",
    "how_much_do_you_agree_with_the_following_statements_2",
    "how_much_do_you_agree_with_the_following_statements_3",
    "how_much_do_you_agree_with_the_following_statements_4",
    "how_much_do_you_agree_with_the_following_statements_5"
  )
  
  free_text_columns <- c(
    "how_is_the_course_meeting_your_expectations_for_what_you_hoped_to_learn_or_experience_optional",
    "why_is_this_your_preferred_way_of_learning",
    "optional_if_you_re_not_taking_this_class_in_your_preferred_learning_method_why",
    "thinking_about_what_helps_you_learn_the_best",
    "what_s_been_your_favorite_part_of_the_class_optional",
    "what_s_been_the_least_enjoyable_part_optional",
    "what_s_the_greatest_challenge_in_meeting_new_people_optional",
    "please_remark_on_aspects_of_the_class_that_make_it_welcoming_optional",
    "what_were_your_expectations_hopes_for_interacting_with_the_other_students_optional",
    "what_were_your_expectations_hopes_for_interacting_with_the_professor_optional",
    "any_other_comments_optional"
  )
  
  df <- df %>%
    normalize_likert_scales(likert_columns) %>%
    parse_discord_responses() %>%
    clean_free_text(free_text_columns)
  
  return(df)
}
