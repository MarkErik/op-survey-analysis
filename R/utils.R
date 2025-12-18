# Utility functions for the Shiny app

#' Clean and format text responses
#' @param text Input text
#' @return Cleaned text
clean_text <- function(text) {
  if (is.null(text) || text == "") {
    return("No response provided")
  }
  
  # Remove extra whitespace
  text <- str_trim(text)
  
  # Handle common HTML entities
  text <- str_replace_all(text, "&nbsp;", " ")
  text <- str_replace_all(text, "&", "&")
  text <- str_replace_all(text, "<", "<")
  text <- str_replace_all(text, ">", ">")
  
  return(text)
}

#' Get response statistics
#' @param df Data frame
#' @param question Question column name
#' @return Statistics data frame
get_response_stats <- function(df, question) {
  if (is.null(df) || !question %in% names(df)) {
    return(NULL)
  }
  
  stats <- df %>%
    filter(!is.na(!!sym(question)) & !!sym(question) != "") %>%
    summarise(
      total_responses = n(),
      avg_response_length = mean(nchar(!!sym(question)), na.rm = TRUE),
      min_response_length = min(nchar(!!sym(question)), na.rm = TRUE),
      max_response_length = max(nchar(!!sym(question)), na.rm = TRUE)
    )
  
  return(stats)
}

#' Extract keywords from responses
#' @param df Data frame
#' @param question Question column name
#' @param top_n Number of top keywords to return
#' @return Data frame of keywords and their frequencies
extract_keywords <- function(df, question, top_n = 10) {
  if (is.null(df) || !question %in% names(df)) {
    return(NULL)
  }
  
  # Get non-empty responses
  responses <- df %>%
    filter(!is.na(!!sym(question)) & !!sym(question) != "") %>%
    pull(!!sym(question))
  
  # Combine all responses
  all_text <- paste(responses, collapse = " ")
  
  # Clean and tokenize
  words <- str_to_lower(all_text) %>%
    str_replace_all("[^a-zA-Z0-9\\s]", "") %>%
    str_split("\\s+") %>%
    unlist() %>%
    str_trim() %>%
    discard(~ . == "")
  
  # Remove common stop words
  stop_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "do", "does", "did", "will", "would", "could", "should", "may", "might", "must", "can", "this", "that", "these", "those", "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them", "my", "your", "his", "its", "our", "their", "mine", "yours", "hers", "ours", "theirs", "am", "as", "from", "about", "into", "through", "during", "before", "after", "above", "below", "up", "down", "out", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "don", "now", "d", "ll", "m", "o", "re", "ve", "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", "hasn", "haven", "isn", "ma", "mightn", "mustn", "needn", "shan", "shouldn", "wasn", "weren", "won", "wouldn")
  
  words <- words[!words %in% stop_words]
  
  # Count word frequencies
  word_counts <- table(words) %>%
    as.data.frame() %>%
    arrange(desc(Freq)) %>%
    head(top_n)
  
  return(word_counts)
}

#' Format date/time for display
#' @param datetime Input datetime
#' @return Formatted datetime string
format_datetime <- function(datetime) {
  if (is.null(datetime)) {
    return("N/A")
  }
  
  tryCatch({
    format(as.POSIXct(datetime), "%Y-%m-%d %H:%M:%S")
  }, error = function(e) {
    as.character(datetime)
  })
}

#' Check if data is loaded
#' @return Boolean indicating if data is loaded
is_data_loaded <- function() {
  !is.null(load_data())
}

#' Get error message for data loading
#' @return Error message or NULL if no error
get_data_error <- function() {
  tryCatch({
    load_data()
    NULL
  }, error = function(e) {
    e$message
  })
}