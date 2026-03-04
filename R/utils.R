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
  if (is.null(discord_string) || discord_string == "") {
    return(setNames(rep(FALSE, length(DISCORD_OPTIONS)), DISCORD_OPTIONS))
  }

  discord_string <- as.character(discord_string)

  discord_string <- trimws(discord_string)

  options <- strsplit(discord_string, ";")[[1]]

  options <- trimws(options)

  result <- setNames(rep(FALSE, length(DISCORD_OPTIONS)), DISCORD_OPTIONS)

  for (option in options) {
    for (i in seq_along(DISCORD_OPTIONS)) {
      if (tolower(option) == tolower(DISCORD_OPTIONS[i])) {
        result[[i]] <- TRUE
        break
      }
    }
  }

  return(result)
}

create_participant_id <- function(timestamp, section, sequence) {
  if (is.null(timestamp) || timestamp == "") {
    return(paste0("P", sprintf("%03d", sequence)))
  }

  if (is.null(section) || section == "") {
    return(paste0("P", sprintf("%03d", sequence)))
  }

  section_match <- regmatches(section, regexpr("[0-9]+", section, perl = TRUE))

  if (length(section_match) == 0) {
    return(paste0("P", sprintf("%03d", sequence)))
  }

  course_number <- section_match[1]

  participant_id <- paste0("P", course_number, sprintf("%03d", sequence))

  return(participant_id)
}

clean_text_field <- function(text) {
  if (is.null(text) || text == "") {
    return("")
  }

  text <- as.character(text)

  text <- gsub("[[:space:]]+", " ", text)

  text <- trimws(text)

  text <- gsub("\n", " ", text)

  text <- gsub(" +", " ", text)

  return(text)
}

format_section_name <- function(section) {
  if (is.null(section) || section == "") {
    return("Unknown Section")
  }

  if (grepl("^\\d+ - ", section)) {
    return(section)
  }

  section_match <- regmatches(section, regexpr("[0-9]+", section, perl = TRUE))

  if (length(section_match) == 0) {
    return(section)
  }

  course_number <- section_match[1]

  time_match <- regmatches(section, regexpr("[a-zA-Z]+", section, perl = TRUE))

  if (length(time_match) == 0) {
    return(section)
  }

  time_portion <- time_match[1]

  formatted <- paste0(course_number, " - ", time_portion)

  return(formatted)
}

calculate_descriptive_stats <- function(x) {
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

  valid_x <- x[!is.na(x)]

  stats <- list(
    n = length(valid_x),
    mean = mean(valid_x, na.rm = TRUE),
    median = median(valid_x, na.rm = TRUE),
    mode = {
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

truncate_text <- function(text, max_length = 100) {
  if (is.null(text) || text == "") {
    return("")
  }

  text <- as.character(text)

  if (nchar(text) <= max_length) {
    return(text)
  }

  truncated <- substr(text, 1, max_length)
  return(paste0(truncated, "..."))
}

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

detect_sentiment <- function(text) {
  if (is.null(text) || text == "") {
    return("neutral")
  }

  text <- tolower(as.character(text))

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
