test_that("ContextAnalyzer can analyze environment", {
  # Create an analyzer with default settings
  analyzer <- ContextAnalyzer$new()
  
  # Analyze the environment
  analyzer$analyze_environment()
  
  # Check that the analyzer contains expected fields
  expect_true(is.list(analyzer$context_data))
  expect_true(is.list(analyzer$context_data$data_frames))
  expect_true(is.character(analyzer$context_data$active_packages))
  expect_true(is.list(analyzer$context_data$environment_info))
})

test_that("ContextAnalyzer respects privacy settings", {
  # Create an analyzer with strict privacy settings
  analyzer <- ContextAnalyzer$new(list(
    analyze_data_frames = FALSE,
    analyze_packages = TRUE,
    analyze_history = FALSE,
    anonymize_data = TRUE
  ))
  
  # Analyze the environment
  analyzer$analyze_environment()
  
  # Check that data frames are not analyzed
  expect_length(analyzer$context_data$data_frames, 0)
  
  # But packages should be analyzed
  expect_true(length(analyzer$context_data$active_packages) > 0)
  
  # And command history should not be analyzed
  expect_length(analyzer$context_data$command_history, 0)
})

test_that("format_context_for_prompt returns a string", {
  # Create sample test data
  df_test <- data.frame(x = 1:5, y = letters[1:5])
  
  # Create an analyzer
  analyzer <- ContextAnalyzer$new()
  
  # Analyze the environment
  analyzer$analyze_environment()
  
  # Format the context for a prompt
  sample_metadata <- list(
    name = "mean",
    package = "base",
    signature = "mean(x, na.rm = FALSE)",
    description = "Calculates the mean of a numeric vector",
    args = c("x", "na.rm")
  )
  
  context_str <- analyzer$format_context_for_prompt("mean", sample_metadata)
  
  # Check that the result is a non-empty string
  expect_true(is.character(context_str))
  expect_true(nchar(context_str) > 0)
})

test_that("build_prompt includes context data when provided", {
  # Create a sample prompt with context data
  sample_metadata <- list(
    name = "mean",
    package = "base",
    signature = "mean(x, na.rm = FALSE)",
    description = "Calculates the mean of a numeric vector",
    args = c("x", "na.rm")
  )
  
  # Build prompt without context
  prompt_without_context <- build_prompt("mean", sample_metadata, FALSE, 2)
  
  # Build prompt with context
  context_data <- "USER ENVIRONMENT CONTEXT:\nActive packages: base, stats\nAvailable data frames: test_df"
  prompt_with_context <- build_prompt("mean", sample_metadata, FALSE, 2, context_data)
  
  # Check that context data is included when provided
  expect_true(grepl("Context awareness mode is set to: YES", prompt_with_context))
  expect_true(grepl("USER ENVIRONMENT CONTEXT:", prompt_with_context))
  
  # Check that context data is not included when not provided
  expect_true(grepl("Context awareness mode is set to: NO", prompt_without_context))
  expect_false(grepl("USER ENVIRONMENT CONTEXT:", prompt_without_context))
})

test_that("tldr_context_config updates configuration correctly", {
  # Save original settings to restore later
  original_config <- get_config_all()
  on.exit({
    # Restore original settings
    if ("context_settings" %in% names(original_config)) {
      tldr_config_internal <- get("tldr_config", envir = asNamespace("tldrAI"))
      tldr_config_internal(context_settings = original_config$context_settings)
    }
  })
  
  # Test updating settings
  result <- tldr_context_config(
    enable_context_awareness = TRUE,
    analyze_data_frames = FALSE,
    max_rows_sample = 10
  )
  
  # Get updated config
  config <- get_config_all()
  
  # Check that settings were updated correctly
  expect_true(is.list(config$context_settings))
  expect_true(config$context_settings$enable_context_awareness)
  expect_false(config$context_settings$analyze_data_frames)
  expect_equal(config$context_settings$max_rows_sample, 10)
  
  # Check that other settings have default values
  expect_true(config$context_settings$analyze_packages)
  expect_true(config$context_settings$anonymize_data)
})

# Helper function to access tldr internals for testing
get_private_function <- function(func_name) {
  get(func_name, envir = asNamespace("tldrAI"))
}