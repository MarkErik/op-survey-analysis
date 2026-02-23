# Data Processing Tests
# Unit tests for data processing utility functions
#
# @author Course Instructor
# @version 2.0.0

# Test setup
library(testthat)

# Load source files
source("../R/utils_data_processing.R")
source("../R/utils_statistics.R")

# Load test data
test_data_path <- file.path("..", "survey_data", "CPSC Experience Survey.csv")

# Create sample test data
create_test_data <- function() {
  data.frame(
    response_id = 1:10,
    section = c("231 - 1pm", "231 - 1pm", "231 - 11am", "217 - 3pm", "231 - 1pm",
                "217 - 11am", "231 - 3pm", "231 - 1pm", "217 - 3pm", "231 - 11am"),
    experience = c("Highly experienced", "No experience", "Took programming",
                   "Highly experienced", "No experience", "Took programming",
                   "Highly experienced", "No experience", "Took programming", "Highly experienced"),
    preference = c("In-person", "Online", "No preference", "In-person", "Online",
                   "No preference", "In-person", "Online", "No preference", "In-person"),
    q1 = c("5 - Strongly Agree", "4", "3", "5", "2", "4", "5", "3", "4", "5"),
    q2 = c("4", "3 - Neutral", "2", "4", "1", "3", "4", "2", "3", "4"),
    q3 = c("5", "4", "3", "5", "2", "4", "5", "3", "4", "5"),
    stringsAsFactors = FALSE
  )
}

# Test suite for data processing functions
context("Data Processing Functions")

# Test parse_likert function
test_that("parse_likert correctly extracts numeric values", {
  expect_equal(parse_likert("5 - Strongly Agree"), 5L)
  expect_equal(parse_likert("3"), 3L)
  expect_equal(parse_likert("1 - Strongly Disagree"), 1L)
  expect_equal(parse_likert(""), NA_integer_)
  expect_equal(parse_likert(NA), NA_integer_)
})

test_that("parse_likert handles edge cases", {
  expect_equal(parse_likert("10"), NA_integer_)
  expect_equal(parse_likert("0"), NA_integer_)
  expect_equal(parse_likert("abc"), NA_integer_)
})

# Test clean_text function
test_that("clean_text removes extra whitespace", {
  expect_equal(clean_text("  hello  world  "), "hello world")
  expect_equal(clean_text("multiple   spaces"), "multiple spaces")
  expect_equal(clean_text(NA), NA_character_)
})

test_that("clean_text handles edge cases", {
  expect_equal(clean_text(""), NA_character_)
  expect_equal(clean_text("   "), NA_character_)
})

# Test extract_section_info function
test_that("extract_section_info parses section correctly", {
  result <- extract_section_info("231 - 1pm")
  expect_equal(result$course_number, "231")
  expect_equal(result$time_slot, "1pm")

  result <- extract_section_info("217 - 3pm")
  expect_equal(result$course_number, "217")
  expect_equal(result$time_slot, "3pm")
})

test_that("extract_section_info handles empty input", {
  result <- extract_section_info("")
  expect_true(is.na(result$course_number))
  expect_true(is.na(result$time_slot))

  result <- extract_section_info(NA)
  expect_true(is.na(result$course_number))
  expect_true(is.na(result$time_slot))
})

# Test filter_section function
test_that("filter_section filters data correctly", {
  test_df <- create_test_data()

  # Filter by section
  filtered <- filter_section(test_df, "231 - 1pm")
  expect_equal(nrow(filtered), 4)
  expect_true(all(filtered$section == "231 - 1pm"))

  # No filter returns all data
  all_data <- filter_section(test_df, NULL)
  expect_equal(nrow(all_data), nrow(test_df))

  # Empty filter returns all data
  all_data <- filter_section(test_df, "")
  expect_equal(nrow(all_data), nrow(test_df))
})

# Test get_likert_cols function
test_that("get_likert_cols identifies Likert columns", {
  test_df <- create_test_data()
  # Add an extra column to make 8 columns total
  test_df$extra_col <- "test"
  colnames(test_df) <- c("response_id", "section", "experience", "preference",
                         "How much do you agree? [Q1]",
                         "How much do you agree? [Q2]",
                         "How much do you agree? [Q3]",
                         "Name")

  likert_cols <- get_likert_cols(test_df)
  expect_true("How much do you agree? [Q1]" %in% likert_cols)
  expect_true("How much do you agree? [Q2]" %in% likert_cols)
  expect_true("How much do you agree? [Q3]" %in% likert_cols)
  expect_false("Name" %in% likert_cols)
})

# Test calculate_response_rates function
test_that("calculate_response_rates computes rates correctly", {
  test_df <- create_test_data()
  test_df$q1[1:3] <- NA

  rates <- calculate_response_rates(test_df, c("q1", "q2"))

  expect_equal(rates$question[rates$question == "q1"], "q1")
  expect_equal(rates$n_responses[rates$question == "q1"], 7)
  expect_equal(rates$response_rate[rates$question == "q1"], 70, tolerance = 0.1)
})

# Test calculate_category_means function
test_that("calculate_category_means computes means correctly", {
  test_df <- create_test_data()
  test_df$q1_num <- sapply(test_df$q1, parse_likert)
  test_df$q2_num <- sapply(test_df$q2, parse_likert)

  means <- calculate_category_means(test_df, c("q1_num", "q2_num"))

  expect_true("q1_num" %in% means$question)
  expect_true("q2_num" %in% means$question)
  expect_true(all(means$mean >= 1 & means$mean <= 5))
})

# Test analyze_missing_patterns function
test_that("analyze_missing_patterns identifies missing data", {
  test_df <- create_test_data()
  test_df$q1[1:3] <- NA
  test_df$q2[1:5] <- NA

  missing <- analyze_missing_patterns(test_df)

  expect_true("q1" %in% missing$column)
  expect_true("q2" %in% missing$column)
  expect_equal(missing$n_missing[missing$column == "q1"], 3)
  expect_equal(missing$n_missing[missing$column == "q2"], 5)
})

# Test normalize_likert function
test_that("normalize_likert scales values correctly", {
  values <- c(1, 2, 3, 4, 5)
  normalized <- normalize_likert(values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[3], 0.5)
  expect_equal(normalized[5], 1)
})

test_that("normalize_likert handles NA values", {
  values <- c(1, NA, 3, NA, 5)
  normalized <- normalize_likert(values)

  expect_equal(length(normalized), 3)
  expect_equal(normalized[1], 0)
})

# Test create_frequency_table function
test_that("create_frequency_table generates correct frequencies", {
  test_df <- create_test_data()
  test_df$q1_num <- sapply(test_df$q1, parse_likert)

  freq <- create_frequency_table(test_df, "q1_num")

  expect_equal(nrow(freq), 5)
  expect_true("value" %in% colnames(freq))
  expect_true("count" %in% colnames(freq))
  expect_true("percentage" %in% colnames(freq))
  expect_equal(sum(freq$count), 10)
})

# Test detect_outliers function
test_that("detect_outliers identifies outliers correctly", {
  # Normal data
  values <- c(3, 3, 3, 3, 3, 3, 3, 3)
  outliers <- detect_outliers(values)
  expect_equal(sum(outliers), 0)

  # Data with outliers
  values <- c(1, 2, 3, 3, 3, 3, 3, 5)
  outliers <- detect_outliers(values)
  expect_true(any(outliers))
})

test_that("detect_outliers handles edge cases", {
  # Too few values
  values <- c(1, 2)
  outliers <- detect_outliers(values)
  expect_equal(length(outliers), 2)

  # NA values
  values <- c(1, NA, 3, NA, 5)
  outliers <- detect_outliers(values)
  expect_equal(sum(outliers), 0)
})

# Test aggregate_by_section function
test_that("aggregate_by_section computes section aggregates", {
  test_df <- create_test_data()
  test_df$q1_num <- sapply(test_df$q1, parse_likert)

  agg <- aggregate_by_section(test_df, c("q1_num"))

  expect_true("What section are you in?" %in% colnames(agg))
  expect_true(nrow(agg) > 0)
})

# Test process_discord function
test_that("process_discord creates indicator columns", {
  test_df <- data.frame(
    response_id = 1:3,
    "About the class Discord (select all that apply)" = c(
      "I have joined the class Discord;I am active in the class Discord",
      "I have joined the class Discord",
      ""
    ),
    stringsAsFactors = FALSE
  )

  result <- process_discord(test_df)

  expect_true("discord_joined" %in% colnames(result))
  expect_true("discord_active" %in% colnames(result))
  expect_true("discord_useful" %in% colnames(result))
  expect_true(result$discord_joined[1])
  expect_true(result$discord_active[1])
  expect_true(result$discord_joined[2])
  expect_false(result$discord_active[2])
})

# Test reshape_to_long function
test_that("reshape_to_long converts data correctly", {
  test_df <- create_test_data()
  test_df$q1_num <- sapply(test_df$q1, parse_likert)
  test_df$q2_num <- sapply(test_df$q2, parse_likert)

  long_df <- reshape_to_long(test_df, c("response_id"), c("q1_num", "q2_num"))

  expect_true("question" %in% colnames(long_df))
  expect_true("response" %in% colnames(long_df))
  expect_true(nrow(long_df) > nrow(test_df))
})

# Test impute_missing function
test_that("impute_missing fills missing values", {
  test_df <- create_test_data()
  test_df$q1_num <- sapply(test_df$q1, parse_likert)
  test_df$q1_num[1:3] <- NA

  imputed <- impute_missing(test_df, "q1_num")

  expect_false(any(is.na(imputed$q1_num)))
})
