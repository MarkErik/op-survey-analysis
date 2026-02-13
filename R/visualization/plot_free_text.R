# Free Text Tab Plot Generation
# Functions for generating Free Text tab visualizations

# Color palette (Anthropic-inspired warm neutrals)
COLORS <- list(
  primary = "#8B7355",
  primary_dark = "#5D4E37",
  primary_light = "#C4A77D",
  secondary = "#6B6B6B",
  secondary_dark = "#4A4A4A",
  secondary_light = "#9E9E9E",
  background = "#FAF8F5",
  surface = "#FFFFFF",
  border = "#E8E4DD",
  text_primary = "#3D3D3D",
  text_secondary = "#6B6B6B",
  text_muted = "#A0A0A0",
  accent = "#D4AF37",
  success = "#7D9A7D",
  warning = "#D4A574",
  error = "#C47D7D",
  info = "#7D9DC4"
)

#' Generate Response Word Cloud
#'
#' Creates a word cloud visualization from free text responses.
#' Shows the most frequently occurring words in the responses.
#'
#' @param df A data frame containing free text responses
#' @param text_column The column name containing the text responses
#' @param max_words Maximum number of words to display (default: 50)
#' @param min_freq Minimum frequency for a word to be included (default: 2)
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_response_wordcloud <- function(df, text_column, max_words = 50, min_freq = 2) {
  if (is.null(df) || !text_column %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Extract text and clean it
  text_data <- df %>%
    filter(!is.na(!!sym(text_column)) & !!sym(text_column) != "") %>%
    pull(!!sym(text_column))
  
  if (length(text_data) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No responses available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Combine all text and tokenize
  all_text <- paste(text_data, collapse = " ")
  
  # Tokenize and count words
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- gsub("[^a-z]", "", words)
  words <- words[nchar(words) > 2]  # Remove very short words
  
  # Common stop words to exclude
  stop_words <- c("the", "and", "for", "are", "but", "not", "you", "all", "can", "had",
                 "her", "was", "one", "our", "out", "has", "have", "been", "this", "that",
                 "with", "they", "from", "what", "which", "their", "there", "would", "about",
                 "more", "very", "when", "will", "just", "some", "like", "than", "into",
                 "only", "could", "them", "who", "get", "its", "also", "because", "should",
                 "being", "were", "did", "each", "most", "such", "your", "how", "after",
                 "other", "been", "make", "time", "first", "may", "any", "same", "do",
                 "course", "class", "think", "feel", "really", "much", "even", "way",
                 "well", "good", "great", "lot", "little", "bit", "something", "things")
  
  words <- words[!words %in% stop_words]
  
  if (length(words) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No meaningful words found", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Count word frequencies
  word_counts <- as.data.frame(table(words), stringsAsFactors = FALSE)
  names(word_counts) <- c("word", "count")
  word_counts <- word_counts %>%
    filter(count >= min_freq) %>%
    arrange(desc(count)) %>%
    head(max_words)
  
  if (nrow(word_counts) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No words meet frequency threshold", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Create word cloud-style bar chart (since wordcloud package may not be available)
  word_counts <- word_counts %>%
    mutate(
      word = factor(word, levels = rev(word$word)),
      size = sqrt(count) * 3
    )
  
  p <- ggplot(word_counts, aes(x = word, y = count)) +
    geom_col(fill = COLORS$primary, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = count), hjust = -0.2, size = 3.5, color = COLORS$text_primary) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 11, color = COLORS$text_primary),
      axis.text.y = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_text(size = 13, color = COLORS$text_secondary, margin = margin(t = 10)),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.margin = margin(10, 20, 10, 10)
    ) +
    labs(y = "Word Frequency")
  
  return(list(plot = p, data = word_counts))
}

#' Generate Response Themes Plot
#'
#' Creates a thematic bar chart showing common themes in free text responses.
#' Themes are identified based on keyword matching.
#'
#' @param df A data frame containing free text responses
#' @param text_column The column name containing the text responses
#' @param themes A named list where names are theme labels and values are keyword vectors
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_response_themes_plot <- function(df, text_column, themes = NULL) {
  if (is.null(df) || !text_column %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Extract text
  text_data <- df %>%
    filter(!is.na(!!sym(text_column)) & !!sym(text_column) != "") %>%
    pull(!!sym(text_column))
  
  if (length(text_data) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No responses available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Default themes if none provided
  if (is.null(themes)) {
    themes <- list(
      "Learning" = c("learn", "understand", "knowledge", "concept", "grasp", "comprehend"),
      "Difficulty" = c("hard", "difficult", "challenging", "struggle", "confusing", "complex"),
      "Engagement" = c("interesting", "engaging", "fun", "enjoy", "exciting", "motivated"),
      "Pacing" = c("fast", "slow", "pace", "speed", "time", "deadline"),
      "Support" = c("help", "support", "assist", "guidance", "feedback", "resources"),
      "Structure" = c("organized", "structure", "clear", "well", "layout", "format"),
      "Content" = c("material", "content", "topic", "subject", "information", "example"),
      "Interaction" = c("discuss", "interact", "collaborate", "group", "team", "partner")
    )
  }
  
  # Count theme occurrences
  theme_counts <- sapply(themes, function(keywords) {
    sum(sapply(text_data, function(text) {
      any(sapply(keywords, function(kw) grepl(kw, tolower(text), fixed = TRUE)))
    }))
  })
  
  theme_df <- data.frame(
    theme = names(theme_counts),
    count = as.numeric(theme_counts),
    stringsAsFactors = FALSE
  ) %>%
    filter(count > 0) %>%
    arrange(desc(count)) %>%
    mutate(
      theme = factor(theme, levels = rev(theme)),
      percentage = round(count / length(text_data) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    )
  
  if (nrow(theme_df) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No themes identified", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Create horizontal bar chart
  p <- ggplot(theme_df, aes(x = theme, y = count)) +
    geom_col(fill = COLORS$primary, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), hjust = -0.2, size = 3.5, color = COLORS$text_primary) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 11, color = COLORS$text_primary),
      axis.text.y = element_text(size = 11, color = COLORS$text_primary),
      axis.title.x = element_text(size = 13, color = COLORS$text_secondary, margin = margin(t = 10)),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.margin = margin(10, 20, 10, 10)
    ) +
    labs(y = "Number of Responses")
  
  return(list(plot = p, data = theme_df))
}

#' Generate Response Sentiment Plot
#'
#' Creates a sentiment analysis visualization showing the distribution of
#' positive, neutral, and negative sentiment in free text responses.
#'
#' @param df A data frame containing free text responses
#' @param text_column The column name containing the text responses
#' @param positive_words Vector of positive sentiment words
#' @param negative_words Vector of negative sentiment words
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_response_sentiment_plot <- function(df, text_column, 
                                              positive_words = NULL, 
                                              negative_words = NULL) {
  if (is.null(df) || !text_column %in% names(df)) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Extract text
  text_data <- df %>%
    filter(!is.na(!!sym(text_column)) & !!sym(text_column) != "") %>%
    pull(!!sym(text_column))
  
  if (length(text_data) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No responses available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Default sentiment words
  if (is.null(positive_words)) {
    positive_words <- c("good", "great", "excellent", "amazing", "wonderful", "fantastic",
                       "helpful", "useful", "enjoy", "love", "like", "appreciate", "thank",
                       "thanks", "positive", "beneficial", "valuable", "clear", "well",
                       "effective", "successful", "happy", "pleased", "satisfied", "impressed",
                       "easy", "simple", "intuitive", "organized", "structured", "comprehensive")
  }
  
  if (is.null(negative_words)) {
    negative_words <- c("bad", "poor", "terrible", "awful", "horrible", "difficult",
                       "hard", "confusing", "unclear", "frustrating", "annoying", "disappointed",
                       "unhelpful", "useless", "hate", "dislike", "negative", "harmful",
                       "waste", "boring", "tedious", "slow", "fast", "overwhelmed", "lost",
                       "struggle", "struggling", "problem", "issue", "concern", "worry",
                       "complicated", "complex", "messy", "disorganized", "lacking", "missing")
  }
  
  # Calculate sentiment for each response
  sentiment_scores <- sapply(text_data, function(text) {
    text_lower <- tolower(text)
    pos_count <- sum(sapply(positive_words, function(w) 
      length(gregexpr(w, text_lower, fixed = TRUE)[[1]])))
    neg_count <- sum(sapply(negative_words, function(w) 
      length(gregexpr(w, text_lower, fixed = TRUE)[[1]])))
    
    if (pos_count > neg_count) {
      return("Positive")
    } else if (neg_count > pos_count) {
      return("Negative")
    } else {
      return("Neutral")
    }
  })
  
  # Count sentiment distribution
  sentiment_df <- data.frame(
    sentiment = sentiment_scores,
    stringsAsFactors = FALSE
  ) %>%
    count(sentiment, name = "count") %>%
    mutate(
      sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative")),
      percentage = round(count / sum(count) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    ) %>%
    arrange(sentiment)
  
  # Create bar chart with sentiment colors
  sentiment_colors <- c("Positive" = COLORS$success, "Neutral" = COLORS$neutral, 
                        "Negative" = COLORS$error)
  
  p <- ggplot(sentiment_df, aes(x = sentiment, y = count, fill = sentiment)) +
    geom_col(width = 0.7, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), vjust = -0.5, size = 3.5, color = COLORS$text_primary) +
    scale_fill_manual(values = sentiment_colors, name = "Sentiment") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, color = COLORS$text_primary),
      axis.text.y = element_text(size = 12, color = COLORS$text_primary),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14, color = COLORS$text_secondary, margin = margin(r = 10)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Number of Responses")
  
  return(list(plot = p, data = sentiment_df))
}

#' Generate Question Response Counts Plot
#'
#' Creates a bar chart showing the number of responses for each free text question.
#'
#' @param df A data frame containing survey responses
#' @param question_columns A character vector of free text question column names
#' @return A list with plot (ggplot object) and data (data frame used for plotting)
#' @export
generate_question_response_counts_plot <- function(df, question_columns) {
  if (is.null(df) || length(question_columns) == 0) {
    p <- ggplot() +
      geom_blank() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5)
    return(list(plot = p, data = NULL))
  }
  
  # Count responses for each question
  response_counts <- sapply(question_columns, function(col) {
    if (col %in% names(df)) {
      sum(!is.na(df[[col]]) & df[[col]] != "")
    } else {
      0
    }
  })
  
  question_df <- data.frame(
    question = names(response_counts),
    count = as.numeric(response_counts),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      display_name = sapply(question, get_column_display_name),
      display_name = substr(display_name, 1, 60),  # Truncate long names
      display_name = ifelse(nchar(display_name) == 60, paste0(display_name, "..."), display_name),
      percentage = round(count / nrow(df) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    ) %>%
    arrange(desc(count)) %>%
    mutate(
      display_name = factor(display_name, levels = rev(display_name))
    )
  
  # Create horizontal bar chart
  p <- ggplot(question_df, aes(x = display_name, y = count)) +
    geom_col(fill = COLORS$primary, color = COLORS$border, linewidth = 0.5) +
    geom_text(aes(label = label), hjust = -0.2, size = 3, color = COLORS$text_primary) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, color = COLORS$text_primary),
      axis.text.y = element_text(size = 10, color = COLORS$text_primary),
      axis.title.x = element_text(size = 13, color = COLORS$text_secondary, margin = margin(t = 10)),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$surface, color = NA),
      plot.background = element_rect(fill = COLORS$surface, color = NA),
      plot.margin = margin(10, 30, 10, 10)
    ) +
    labs(y = "Number of Responses")
  
  return(list(plot = p, data = question_df))
}
