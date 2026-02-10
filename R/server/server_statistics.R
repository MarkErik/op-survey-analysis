# Server Module - Statistics Handling

# Update statistics on home page
update_statistics <- function(df, free_text_questions) {
  # Calculate statistics
  stats <- calculate_statistics(df, free_text_questions)
  
  # Return statistics as a list
  list(
    total_responses = stats$total_responses,
    question_count = stats$question_count
  )
}