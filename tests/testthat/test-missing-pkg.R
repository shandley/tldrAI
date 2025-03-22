test_that("missing package handling works", {
  # This test just validates that the build_prompt function handles missing package metadata correctly
  # Create mock metadata for a missing package
  mock_metadata <- list(
    name = "test_function",
    package = "test_package",
    missing_package = TRUE,
    install_command = 'install.packages("test_package")',
    signature = "test_function(...)"
  )
  
  # Mock the functions we're testing
  old_build_prompt <- tldrAI:::build_prompt
  old_get_missing_package_template <- tldrAI:::get_missing_package_template
  
  # Define a simple mock template function
  mock_template <- function() {
    return("Template for {{PACKAGE_NAME}} package with {{FUNCTION_NAME}} function using {{INSTALL_COMMAND}}")
  }
  
  # Temporarily assign our mock functions
  assignInNamespace("get_missing_package_template", mock_template, "tldrAI")
  
  # Verify that build_prompt uses the appropriate template for missing packages
  result <- tldrAI:::build_prompt("test_function", mock_metadata, FALSE, 2)
  expect_match(result, "Template for test_package package", fixed = TRUE)
  expect_match(result, "test_function", fixed = TRUE)
  expect_match(result, 'install.packages("test_package")', fixed = TRUE)
  
  # Restore the original functions
  assignInNamespace("get_missing_package_template", old_get_missing_package_template, "tldrAI")
})