context("Error analysis functions")

test_that("process_error_input handles string errors", {
  # Create a simple string error message
  error_msg <- "Error in mean(x) : object 'x' not found"
  
  # Process it
  result <- process_error_input(error_msg)
  
  # Check structure and content
  expect_type(result, "list")
  expect_true("message" %in% names(result))
  expect_equal(result$message, error_msg)
  expect_null(result$call)
  expect_null(result$traceback)
})

test_that("process_error_input handles error objects", {
  # Create an error object via tryCatch
  error_obj <- tryCatch(
    mean(nonexistent_var),
    error = function(e) e
  )
  
  # Process it
  result <- process_error_input(error_obj)
  
  # Check structure and content
  expect_type(result, "list")
  expect_true("message" %in% names(result))
  expect_true(grepl("nonexistent_var", result$message))
  expect_false(is.null(result$call))
})

test_that("build_error_prompt generates correct prompt", {
  # Create error info
  error_info <- list(
    message = "Error in mean(x) : object 'x' not found",
    call = quote(mean(x)),
    traceback = NULL
  )
  
  # Code that caused the error
  code <- "mean(x)"
  
  # Build prompt
  prompt <- build_error_prompt(error_info, code, TRUE, 2)
  
  # Check prompt content
  expect_true(grepl(error_info$message, prompt, fixed = TRUE))
  expect_true(grepl(code, prompt, fixed = TRUE))
  expect_true(grepl("RECOMMEND}}: YES", prompt, fixed = TRUE))
  expect_true(grepl("EXAMPLES_REQUESTED}}: 2", prompt, fixed = TRUE))
})

test_that("extract_code_block extracts content correctly", {
  # Create a test response with a code block
  response <- "Some text before
```
# Error Analysis
## What Went Wrong
The error means X.
```
Some text after"
  
  # Extract the code block
  result <- extract_code_block(response)
  
  # Check result
  expect_true(grepl("# Error Analysis", result))
  expect_true(grepl("## What Went Wrong", result))
  expect_false(grepl("```", result))
})

test_that("capture_last_error returns NULL when no error", {
  # Clear any existing error messages
  try(invisible(NULL), silent = TRUE)
  
  # Capture with no recent error
  result <- capture_last_error()
  
  # Should be NULL
  expect_null(result)
})

test_that("format_content formats headings correctly", {
  # Create test content with markdown
  content <- "# Main Heading\n## Subheading\n- List item 1\n- List item 2"
  
  # Format it
  result <- format_content(content)
  
  # Should contain formatting
  expect_true(grepl("Main Heading", result))
  expect_true(grepl("Subheading", result))
  expect_true(grepl("•", result))  # Bullet points converted
})