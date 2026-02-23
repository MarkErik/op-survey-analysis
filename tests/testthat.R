# Test Setup for CPSC Experience Survey Explorer
# This file is executed when running tests
#
# @author Course Instructor
# @version 2.0.0

# Load testthat
library(testthat)

# Load the application source files
source("../global.R")

# Load utility functions
source("../R/utils_data_processing.R")
source("../R/utils_visualization.R")
source("../R/utils_statistics.R")

# Set testthat options
testthat::test_check("op-survey-analysis")

# Custom testthat reporter with more detail
testthat::test_dir(
  ".",
  reporter = testthat::ListReporter,
  stop_on_failure = FALSE
)
