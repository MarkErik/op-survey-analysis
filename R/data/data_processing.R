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
#' question text for UI display. This function has a side effect of
#' creating a global `column_mappings` variable.
#'
#' @param df A data frame with normalized column names
#' @return The data frame with the temporary attribute removed
#' @export
build_column_mappings <- function(df) {
  original_names <- attr(df, "original_column_names")
  if (is.null(original_names)) {
    original_names <- names(df)
  }
  
  column_mappings <<- list(
    original = original_names,
    normalized = names(df)
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

#' Normalize Likert Scales
#'
#' Strips non-numeric characters from Likert responses to extract numeric rating (1-5).
#'
#' @param df A data frame
#' @param likert_columns Character vector of column names containing Likert responses
#' @return A data frame with Likert columns converted to integers
#' @export
normalize_likert_scales <- function(df, likert_columns) {
  df <- df %>%
    mutate(across(
      all_of(likert_columns),
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
  df <- df %>%
    mutate(across(
      all_of(text_columns),
      ~ {
        if (is.na(.x)) return(NA_character_)
        .x %>%
          str_trim() %>%
          gsub("\\r\\n|\\r|\\n", " ", .) %>%
          gsub("\\s+", " ", .) %>%
          str_trim()
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
    generate_participant_ids() %>%
    parse_section_identifiers()
  
  # Define column groups
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
