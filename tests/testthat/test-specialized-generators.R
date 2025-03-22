# Test for stringr and lubridate specialized generators

test_that("stringr and lubridate packages are in common packages list", {
  # Check metadata.R for common packages reference
  metadata_file <- readLines("/Users/scott/gdrive/code/R/tldrAI/R/metadata.R")
  common_packages_line <- grep("common_packages <- c\\(", metadata_file, value = TRUE)
  
  # Verify that stringr and lubridate are in the common packages list
  expect_true(grepl("stringr", common_packages_line))
  expect_true(grepl("lubridate", common_packages_line))
  
  # Check for suggested packages in ambiguity resolution
  potential_pkg_line <- grep("potential_pkg in c\\(", metadata_file, value = TRUE)
  expect_true(grepl("stringr", potential_pkg_line))
  expect_true(grepl("lubridate", potential_pkg_line))
})

test_that("context.R contains stringr generator reference", {
  # Check context.R for stringr generator references
  context_file <- readLines("/Users/scott/gdrive/code/R/tldrAI/R/context.R")
  
  # Check for stringr condition in the file
  stringr_condition <- grep("func_package == \"stringr\"", context_file, value = TRUE)
  expect_true(length(stringr_condition) > 0)
  
  # Check for stringr functions
  stringr_funcs <- grep("\"str_detect\", \"str_extract\"", context_file, value = TRUE)
  expect_true(length(stringr_funcs) > 0)
  
  # Verify the generate_stringr_example function exists
  stringr_generator <- grep("generate_stringr_example = function", context_file, value = TRUE)
  expect_true(length(stringr_generator) > 0)
})

test_that("context.R contains lubridate generator reference", {
  # Check context.R for lubridate generator references
  context_file <- readLines("/Users/scott/gdrive/code/R/tldrAI/R/context.R")
  
  # Check for lubridate condition in the file
  lubridate_condition <- grep("func_package == \"lubridate\"", context_file, value = TRUE)
  expect_true(length(lubridate_condition) > 0)
  
  # Check for lubridate functions
  lubridate_funcs <- grep("\"ymd\", \"mdy\", \"dmy\"", context_file, value = TRUE)
  expect_true(length(lubridate_funcs) > 0)
  
  # Verify the generate_lubridate_example function exists
  lubridate_generator <- grep("generate_lubridate_example = function", context_file, value = TRUE)
  expect_true(length(lubridate_generator) > 0)
})