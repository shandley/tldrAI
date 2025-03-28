test_that("tldr functions return the expected structure", {
  # Mock the API response
  mockr::with_mock(
    get_ai_response = function(...) "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean (average) of a numeric vector.\n\n## Usage\nmean(x, na.rm = FALSE)\n\n## Key Arguments\n- x: A numeric vector\n- na.rm: Whether to remove NA values before calculation\n\n## Examples\n```r\n# Calculate mean of a vector\nmean(c(1, 2, 3, 4, 5))  # 3\n\n# Calculate mean with NA values by removing them\nmean(c(1, 2, NA, 4, 5), na.rm = TRUE)  # 3\n```\n```",
    {
      # Test the main function
      response <- tldr("mean", refresh = TRUE)
      expect_true(is.character(response))
      expect_match(response, "# mean", fixed = TRUE)
    }
  )
})

test_that("get_function_metadata returns the expected structure", {
  metadata <- get_function_metadata("mean")
  expect_equal(metadata$name, "mean")
  expect_equal(metadata$package, "base")
  expect_true(is.character(metadata$signature))
  expect_true(is.character(metadata$description))
})

test_that("build_prompt includes all required placeholders", {
  template <- get_prompt_template()
  expect_match(template, "{{FUNCTION_NAME}}", fixed = TRUE)
  expect_match(template, "{{FUNCTION_SIGNATURE}}", fixed = TRUE)
  expect_match(template, "{{FUNCTION_DESCRIPTION}}", fixed = TRUE)
  expect_match(template, "{{PACKAGE_NAME}}", fixed = TRUE)
  expect_match(template, "{{EXAMPLES_REQUESTED}}", fixed = TRUE)
  expect_match(template, "{{VERBOSE}}", fixed = TRUE)
})

test_that("missing package templates have required placeholders", {
  template <- get_missing_package_template()
  expect_match(template, "{{FUNCTION_NAME}}", fixed = TRUE)
  expect_match(template, "{{PACKAGE_NAME}}", fixed = TRUE)
  expect_match(template, "{{INSTALL_COMMAND}}", fixed = TRUE)
  
  template <- get_missing_function_template()
  expect_match(template, "{{FUNCTION_NAME}}", fixed = TRUE)
  expect_match(template, "{{PACKAGE_NAME}}", fixed = TRUE)
})

test_that("missing package metadata is handled correctly", {
  # Create mock metadata for a missing package
  mock_metadata <- list(
    name = "some_function",
    package = "nonexistent_package",
    missing_package = TRUE,
    install_command = 'install.packages("nonexistent_package")',
    signature = "some_function(...)"
  )
  
  # Test that build_prompt uses the missing package template
  prompt <- build_prompt("some_function", mock_metadata, FALSE, 2)
  expect_match(prompt, "Package Not Installed:", fixed = TRUE)
  expect_match(prompt, "nonexistent_package", fixed = TRUE)
  expect_match(prompt, 'install.packages("nonexistent_package")', fixed = TRUE)
})
