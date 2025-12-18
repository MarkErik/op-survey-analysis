# Simple R script to import CSV data, load as dataframe, and export as CSV
library(tidyverse)

# Import the CSV file and load as dataframe
df <- read_csv("/Users/mark/Library/CloudStorage/OneDrive-UniversityofCalgary/Online Pathways/CPSC Experience Survey (Responses) - Form Responses.csv")

discord_options <- c(
  "I have joined the class Discord",
  "I am active in the class Discord",
  "It is really useful for my learning",
  "It is the main way that I connect with other students in this class",
  "I like that the class Discord exists",
  "I'm not sure what its purpose is",
  "I don't like the amount of notifications",
  "I wish I had known about it sooner",
  "I don't find it very useful"
)

# Display the dataframe to verify it was loaded correctly
print("Original dataframe contents:")
print(df)

df <- df %>%
  rename(timestamp = Timestamp,
         section = `What section are you in?`,
         prior_experience = `Prior to taking this course, what was your programming experience?`,
         learning_preference = `Do you prefer in-person or online learning?`,
         why_learning_preference = `Why is this your preferred way of learning?`,
         discord = `About the class Discord (select all that apply)`,
         free_text_learning_preference = `(Optional) If you're not taking this class in your preferred learning method, why?`,
         free_text_class_interesting_engaging = `Any other comments that you would like to share that you feel would make the class more interesting or engaging for you? (Optional)`,
         free_text_learning_meeting_expectations = `How is the course meeting your expectations for what you hoped to learn or experience? (Optional)`,
         free_text_class_welcoming_inclusive = `Please remark on aspects of the class that make it welcoming and inclusive to you, given your identities and needs, and suggest any aspects that could improve inclusive teaching in this class (Optional)`,
         free_text_hopes_interacting_students = `What were your expectations/hopes for interacting with the other students? If it isn't meeting your wishes, we'd like to hear more. (Optional)`,
         free_text_hopes_interacting_professor = `What were your expectations/hopes for interacting with the professor? If it isn't meeting your wishes, we'd like to hear more. (Optional)`,
         free_text_class_favorite_part = `What's been your favorite part of the class for you so far, and why? (Optional)`,
         free_text_class_least_enjoyable_part = `What's been the least enjoyable part of class for you so far, and why? (Optional)`,
         free_text_more_and_less_of = `Thinking about what helps you learn the best, if you are going to continue taking programming classes after this one:\n- What do you wish the courses would do more of?\n- And also, what do you wish they would do less of?\n(Optional)`,
         free_text_challenge_meeting_people = `What's the greatest challenge in meeting new people at university? (Optional)`
  ) %>%
  mutate(
    discord = str_replace_all(
      discord,
      fixed("It is really useful for me for learning"),
      "It is really useful for my learning"
    )
  ) %>%
  rename_with(~ gsub(
    "^How much do you agree with the statement\\? \\[(.*)\\]$",
    "(course) \\1",
    .x
  )) %>%
  rename_with(~ gsub(
    "^How much do you agree with the following statements?\\? \\[(.*)\\]$",
    "(community) \\1",
    .x
  )) %>%
  rename_with(~ gsub(
    "^How much do the following elements contribute to your learning?\\? \\[(.*)\\]$",
    "(learning) \\1",
    .x
  )) %>%
  mutate(
    across(
      .cols = discord,
      .fns  = as.character
    )
  ) %>%
  mutate(
    discord_other_text = discord
  )

for (opt in discord_options) {
  
  col_name <- paste0(
    "discord_",
    opt |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[^a-z0-9]+", "_") |>
      stringr::str_replace_all("^_+|_+$", "")
  )
  
  df[[col_name]] <- as.integer(stringr::str_detect(df$discord, stringr::fixed(opt)))
}

for (opt in discord_options) {
  df$discord_other_text <- str_remove_all(
    df$discord_other_text,
    paste0("\\s*,?\\s*", fixed(opt), "\\s*,?\\s*")
  )
}

df <- df %>%
  mutate(
    discord_other_text = str_squish(discord_other_text),
    discord_other_text = na_if(discord_other_text, ""),
    discord_other_selected = as.integer(!is.na(discord_other_text))
  )

# Export the dataframe to a new CSV file
output_csv_file <- "exported_data.csv"
write.csv(df, file = output_csv_file, row.names = FALSE)

cat(paste("Data successfully exported to", output_csv_file, "\n"))
