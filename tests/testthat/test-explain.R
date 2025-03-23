test_that("tldr_explain handles basic inputs correctly", {
  # Skip if offline or no API keys
  skip_if(get_config("offline_mode", default = FALSE), "Offline mode enabled, skipping API tests")
  skip_if_not_installed("testthat")
  
  # Create test code
  test_code <- "x <- 1:10\nmean(x)"
  
  # Mock the API response since we can't actually call it in tests
  mockery::stub(tldr_explain, "get_ai_response", "Mocked explanation")
  mockery::stub(tldr_explain, "print_explanation_response", NULL)
  
  # Test basic functionality
  result <- tldr_explain(test_code, detail_level = "basic")
  expect_type(result, "character")
  
  # Test with different detail levels
  result <- tldr_explain(test_code, detail_level = "intermediate")
  expect_type(result, "character")
  
  result <- tldr_explain(test_code, detail_level = "advanced")
  expect_type(result, "character")
  
  # Test with invalid detail level (should warn and use default)
  expect_warning(
    result <- tldr_explain(test_code, detail_level = "invalid"),
    "Invalid detail_level"
  )
  expect_type(result, "character")
})

test_that("tldr_explain handles file inputs", {
  # Skip if offline or no API keys
  skip_if(get_config("offline_mode", default = FALSE), "Offline mode enabled, skipping API tests")
  
  # Create a temporary R file
  tmp_file <- tempfile(fileext = ".R")
  write("x <- 1:10\nmean(x)", tmp_file)
  on.exit(unlink(tmp_file))
  
  # Mock the API response
  mockery::stub(tldr_explain, "get_ai_response", "Mocked file explanation")
  mockery::stub(tldr_explain, "print_explanation_response", NULL)
  
  # Test with file input
  result <- tldr_explain(tmp_file)
  expect_type(result, "character")
})

test_that("extract_variables_from_code works correctly", {
  # Test basic variable extraction
  code <- "x <- 1:10\ny <- mean(x)\nz <- y * 2"
  vars <- extract_variables_from_code(code)
  expect_true(all(c("x", "y", "z") %in% vars))
  
  # Test with packages and functions
  code <- "library(dplyr)\nmtcars %>% filter(mpg > 20) %>% summarise(avg = mean(mpg))"
  vars <- extract_variables_from_code(code)
  expect_true("mtcars" %in% vars)
  expect_true("avg" %in% vars)
  expect_true("mpg" %in% vars)
})

test_that("extract_functions_from_code works correctly", {
  # Test basic function extraction
  code <- "x <- 1:10\ny <- mean(x)\nz <- sd(x) + sum(x)"
  funcs <- extract_functions_from_code(code)
  expect_true(all(c("mean", "sd", "sum") %in% funcs))
  
  # Test with packages and pipes
  code <- "library(dplyr)\nmtcars %>% filter(mpg > 20) %>% summarise(avg = mean(mpg))"
  funcs <- extract_functions_from_code(code)
  expect_true(all(c("library", "filter", "summarise", "mean") %in% funcs))
})