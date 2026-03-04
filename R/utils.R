extract_likert_value <- function(response) {
  if (is.null(response) || response == "") {
    return(NA_integer_)
  }

  response <- as.character(response)

  response <- trimws(response)

  match <- regmatches(response, regexpr("[0-9]+", response, perl = TRUE))

  if (length(match) == 0) {
    return(NA_integer_)
  }

  numeric_value <- as.integer(match[1])

  if (numeric_value >= 1 && numeric_value <= 5) {
    return(numeric_value)
  }

  return(NA_integer_)
}


parse_discord_field <- function(discord_string) {
  # Handle NULL or empty input
  if (is.null(discord_string) || discord_string == "") {
    return(setNames(rep(FALSE, length(DISCORD_OPTIONS)), DISCORD_OPTIONS))
  }

  # Convert to character for processing
  discord_string <- as.character(discord_string)

  # Remove leading/trailing whitespace
  discord_string <- trimws(discord_string)

  # Split by semicolon to get individual options
  options <- strsplit(discord_string, ";")[[1]]

  # Clean each option (trim whitespace)
  options <- trimws(options)

  # Create named logical vector initialized to FALSE
  result <- setNames(rep(FALSE, length(DISCORD_OPTIONS)), DISCORD_OPTIONS)

  # Check each option against our predefined list
  for (option in options) {
    # Check if option matches any of our predefined options
    # Use case-insensitive matching
    for (i in seq_along(DISCORD_OPTIONS)) {
      if (tolower(option) == tolower(DISCORD_OPTIONS[i])) {
        result[[i]] <- TRUE
        break
      }
    }
  }

  return(result)
}

# =============================================================================
# Participant ID Generation
# =============================================================================

#' Generate synthetic participant ID
#'
#' Creates a unique participant ID based on timestamp, section, and sequence number.
#' This allows referencing individual responses even though the survey is anonymous.
#'
#' @param timestamp Character string from Timestamp column
#' @param section Character string from Section column
#' @param sequence Integer sequence number for this timestamp/section combination
#' @return Character ID in format "P###" (e.g., "P001", "P002")
#' @examples
#' create_participant_id("2024/01/15 10:30:00 AM EST", "231 - 1pm", 1)  # Returns "P001"
create_participant_id <- function(timestamp, section, sequence) {
  # Validate inputs
  if (is.null(timestamp) || timestamp == "") {
    return(paste0("P", sprintf("%03d", sequence)))
  }

  if (is.null(section) || section == "") {
    return(paste0("P", sprintf("%03d", sequence)))
  }

  # Extract course number from section (e.g., "231" from "231 - 1pm")
  section_match <- regmatches(section, regexpr("[0-9]+", section, perl = TRUE))

  if (length(section_match) == 0) {
    return(paste0("P", sprintf("%03d", sequence)))
  }

  course_number <- section_match[1]

  # Create ID: P + course number + sequence number
  # Format: P231001 for course 231, sequence 1
  participant_id <- paste0("P", course_number, sprintf("%03d", sequence))

  return(participant_id)
}

# =============================================================================
# Free-Text Cleaning
# =============================================================================

#' Clean free-text response
#'
#' Removes extra whitespace, normalizes line breaks, and handles special characters
#' in free-text responses.
#'
#' @param text Character string containing free-text response
#' @return Cleaned character string
#' @examples
#' clean_text_field("  Hello   world  ")  # Returns "Hello world"
#' clean_text_field("Line1\nLine2")       # Returns "Line1 Line2"
clean_text_field <- function(text) {
  # Handle NULL or empty input
  if (is.null(text) || text == "") {
    return("")
  }

  # Convert to character for processing
  text <- as.character(text)

  # Remove extra whitespace (spaces, tabs, newlines)
  # Replace multiple spaces/tabs/newlines with single space
  text <- gsub("[[:space:]]+", " ", text)

  # Trim leading/trailing whitespace
  text <- trimws(text)

  # Replace newlines with spaces for consistency
  text <- gsub("\n", " ", text)

  # Replace multiple spaces with single space (after newline replacement)
  text <- gsub(" +", " ", text)

  return(text)
}

# =============================================================================
# Section Name Formatting
# =============================================================================

#' Format section name for display
#'
#' Formats section identifier for display purposes.
#'
#' @param section Character string from Section column
#' @return Formatted section name
#' @examples
#' format_section_name("231 - 1pm")  # Returns "231 - 1pm"
#' format_section_name("217 - 11am") # Returns "217 - 11am"
format_section_name <- function(section) {
  # Handle NULL or empty input
  if (is.null(section) || section == "") {
    return("Unknown Section")
  }

  # Return as-is if already formatted
  if (grepl("^\\d+ - ", section)) {
    return(section)
  }

  # Try to parse and reformat
  section_match <- regmatches(section, regexpr("[0-9]+", section, perl = TRUE))

  if (length(section_match) == 0) {
    return(section)
  }

  course_number <- section_match[1]

  # Extract time portion (after the dash)
  time_match <- regmatches(section, regexpr("[a-zA-Z]+", section, perl = TRUE))

  if (length(time_match) == 0) {
    return(section)
  }

  time_portion <- time_match[1]

  # Reformat as "COURSE - TIME"
  formatted <- paste0(course_number, " - ", time_portion)

  return(formatted)
}

# =============================================================================
# Statistics Helper Functions
# =============================================================================

#' Calculate descriptive statistics for a numeric vector
#'
#' @param x Numeric vector of values
#' @return Named list with statistics
#' @examples
#' calculate_stats(c(1, 2, 3, 4, 5))  # Returns list with n, mean, median, etc.
calculate_descriptive_stats <- function(x) {
  # Handle NULL or empty input
  if (is.null(x) || length(x) == 0) {
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
      missing = 0
    ))
  }

  # Remove NA values for calculations
  valid_x <- x[!is.na(x)]

  # Calculate statistics
  stats <- list(
    n = length(valid_x),
    mean = mean(valid_x, na.rm = TRUE),
    median = median(valid_x, na.rm = TRUE),
    mode = {
      # Find most frequent value
      freq_table <- table(valid_x)
      names(freq_table)[which.max(freq_table)]
    },
    sd = sd(valid_x, na.rm = TRUE),
    se = sd(valid_x, na.rm = TRUE) / sqrt(length(valid_x)),
    min = min(valid_x, na.rm = TRUE),
    max = max(valid_x, na.rm = TRUE),
    q1 = quantile(valid_x, 0.25, na.rm = TRUE),
    q3 = quantile(valid_x, 0.75, na.rm = TRUE),
    missing = sum(is.na(x))
  )

  return(stats)
}

#' Calculate Cohen's d effect size
#'
#' @param group1 Numeric vector for group 1
#' @param group2 Numeric vector for group 2
#' @return List with effect size and interpretation
#' @examples
#' calculate_cohens_d(c(1,2,3), c(4,5,6))  # Returns effect size and interpretation
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
# Text Processing Helpers
# =============================================================================

#' Truncate text with ellipsis
#'
#' @param text Character string to truncate
#' @param max_length Integer maximum length
#' @return Truncated text with ellipsis if needed
#' @examples
#' truncate_text("Hello world", 5)  # Returns "Hello..."
truncate_text <- function(text, max_length = 100) {
  # Handle NULL or empty input
  if (is.null(text) || text == "") {
    return("")
  }

  text <- as.character(text)

  if (nchar(text) <= max_length) {
    return(text)
  }

  # Truncate and add ellipsis
  truncated <- substr(text, 1, max_length)
  return(paste0(truncated, "..."))
}

#' Count words in text
#'
#' @param text Character string
#' @return Number of words
#' @examples
#' count_words("Hello world")  # Returns 2
count_words <- function(text) {
  if (is.null(text) || text == "") {
    return(0)
  }

  text <- trimws(as.character(text))
  if (text == "") {
    return(0)
  }

  words <- strsplit(text, "\\s+")[[1]]
  return(length(words))
}

#' Detect sentiment (simple positive/negative)
#'
#' @param text Character string
#' @return "positive", "negative", or "neutral"
#' @examples
#' detect_sentiment("I love this class")  # Returns "positive"
detect_sentiment <- function(text) {
  if (is.null(text) || text == "") {
    return("neutral")
  }

  text <- tolower(as.character(text))

  # Simple positive/negative word lists
  positive_words <- c(
    "love", "like", "great", "excellent", "amazing", "wonderful",
    "good", "best", "enjoy", "happy", "satisfied", "excited",
    "helpful", "useful", "important", "essential", "worth"
  )

  negative_words <- c(
    "hate", "dislike", "bad", "terrible", "awful", "worst",
    "poor", "unhappy", "dissatisfied", "disappointed", "frustrated",
    "confusing", "difficult", "hard", "boring", "annoying"
  )

  # Count matches
  positive_count <- sum(positive_words %in% text)
  negative_count <- sum(negative_words %in% text)

  if (positive_count > negative_count) {
    return("positive")
  } else if (negative_count > positive_count) {
    return("negative")
  } else {
    return("neutral")
  }
}
