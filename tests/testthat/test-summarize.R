test_that("tldr_summarize validates inputs correctly", {
  # Test missing file path
  expect_error(tldr_summarize(), "Please provide at least one R script file path")
  
  # Test invalid file path
  expect_error(tldr_summarize("nonexistent.R"), "The following files do not exist:")
  
  # Test invalid focus
  expect_warning(tldr_summarize(system.file("extdata", "example.R", package = "tldrAI"), 
                              focus = "invalid"),
                "Invalid focus: 'invalid'. Using 'general' instead")
  
  # Test invalid detail
  expect_warning(tldr_summarize(system.file("extdata", "example.R", package = "tldrAI"), 
                              detail = "invalid"),
                "Invalid detail: 'invalid'. Using 'standard' instead")
  
  # Test invalid visualization type
  expect_warning(tldr_summarize(system.file("extdata", "example.R", package = "tldrAI"), 
                              visualize = TRUE, vis_type = "invalid"),
                "Invalid visualization type: 'invalid'. Using 'dependency' instead")
  
  # Test invalid output format
  expect_warning(tldr_summarize(system.file("extdata", "example.R", package = "tldrAI"), 
                              output = "invalid"),
                "Invalid output: 'invalid'. Using 'console' instead")
})

test_that("tldr_summarize extracts script metadata correctly", {
  # Create a simple script file for testing
  temp_file <- tempfile(fileext = ".R")
  cat('
  # Example script
  library(dplyr)
  library(ggplot2)
  
  # Load data
  data <- read.csv("data.csv")
  
  # Define a function
  calculate_stats <- function(x) {
    mean_val <- mean(x)
    sd_val <- sd(x)
    return(list(mean = mean_val, sd = sd_val))
  }
  
  # Process data
  result <- data %>%
    filter(!is.na(value)) %>%
    group_by(group) %>%
    summarize(mean = mean(value), sd = sd(value))
  
  # Plot results
  ggplot(result, aes(x = group, y = mean)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))
  ', file = temp_file)
  
  viz_handler <- VisualizationHandler$new()
  metadata <- viz_handler$extract_script_metadata(temp_file)
  
  # Test package extraction
  expect_true("dplyr" %in% metadata[[temp_file]]$packages)
  expect_true("ggplot2" %in% metadata[[temp_file]]$packages)
  
  # Test function extraction
  expect_true("calculate_stats" %in% metadata[[temp_file]]$functions_defined)
  
  # Test function calls extraction
  expect_true("read.csv" %in% metadata[[temp_file]]$functions_called)
  expect_true("mean" %in% metadata[[temp_file]]$functions_called)
  expect_true("sd" %in% metadata[[temp_file]]$functions_called)
  
  # Test variable extraction
  expect_true("data" %in% metadata[[temp_file]]$variables)
  expect_true("result" %in% metadata[[temp_file]]$variables)
  
  # Clean up
  unlink(temp_file)
})

test_that("tldr_summarize handles context information correctly", {
  # Skip actual API calls in tests
  skip_if_not(interactive(), "Skipping interactive test")
  
  # Create mock context information
  # This is testing the internal function that extracts context
  context_info <- get_context_for_scripts(system.file("extdata", "example.R", package = "tldrAI"))
  
  # Test that context information is either NULL or a character string
  expect_true(is.null(context_info) || is.character(context_info))
})

test_that("tldr_summarize generates ASCII visualization correctly", {
  # Create a simple script file for testing
  temp_file <- tempfile(fileext = ".R")
  cat('
  # Example script
  library(dplyr)
  library(ggplot2)
  
  # Define a function
  calculate_stats <- function(x) {
    mean_val <- mean(x)
    sd_val <- sd(x)
    return(list(mean = mean_val, sd = sd_val))
  }
  ', file = temp_file)
  
  viz_handler <- VisualizationHandler$new()
  
  # Test dependency visualization
  dependency_viz <- viz_handler$generate_ascii_script_dependency(temp_file)
  expect_true(is.character(dependency_viz))
  expect_true(grepl("Script Dependencies", dependency_viz))
  expect_true(grepl("dplyr", dependency_viz))
  expect_true(grepl("ggplot2", dependency_viz))
  
  # Test structure visualization
  structure_viz <- viz_handler$generate_ascii_script_structure(temp_file)
  expect_true(is.character(structure_viz))
  expect_true(grepl("Script Structure", structure_viz))
  expect_true(grepl("Functions", structure_viz))
  expect_true(grepl("calculate_stats", structure_viz))
  
  # Clean up
  unlink(temp_file)
})

test_that("tldr_summarize handles code block extraction correctly", {
  # Create a script with multiple code blocks
  temp_file <- tempfile(fileext = ".R")
  cat('
  # Block 1: Imports
  library(dplyr)
  library(ggplot2)
  
  # Block 2: Data loading
  data <- read.csv("data.csv")
  
  # Block 3: Data processing
  result <- data %>%
    filter(!is.na(value)) %>%
    group_by(group) %>%
    summarize(mean = mean(value), sd = sd(value))
  
  # Block 4: Visualization
  plot <- ggplot(result, aes(x = group, y = mean)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))
  ', file = temp_file)
  
  viz_handler <- VisualizationHandler$new()
  blocks <- viz_handler$extract_code_blocks(paste(readLines(temp_file), collapse = "\n"))
  
  # Test that blocks were extracted
  expect_true(length(blocks) >= 3)
  
  # Test that blocks contain expected content
  expect_true(any(grepl("library\\(dplyr\\)", blocks)))
  expect_true(any(grepl("read\\.csv", blocks)))
  expect_true(any(grepl("ggplot", blocks)))
  
  # Clean up
  unlink(temp_file)
})

# Create a mock implementation of tldr_summarize for testing
with_mock_api_response <- function(expr) {
  old_get_ai_response <- get_ai_response
  get_ai_response <<- function(prompt, provider_override = NULL, async = FALSE) {
    return("# R Script Summary\n\n## Overview\nThis script analyzes data and generates visualizations.\n\n## Key Components\n- Data loading functions\n- Analysis utilities\n- Visualization components\n\n## Package Dependencies\ndplyr, ggplot2, and others\n\n## Data Flow\nLoads data → processes → visualizes\n\n## Key Insights\nWell-structured code with modular components")
  }
  on.exit(get_ai_response <<- old_get_ai_response)
  eval(expr)
}

test_that("tldr_summarize returns expected result structure", {
  # Skip if not in interactive mode
  skip_if_not(interactive(), "Skipping interactive test")
  
  # Create a simple script file for testing
  temp_file <- tempfile(fileext = ".R")
  cat('
  # Example script
  library(dplyr)
  library(ggplot2)
  
  # Load data
  data <- read.csv("data.csv")
  ', file = temp_file)
  
  # Test with mocked API response
  with_mock_api_response({
    result <- tldr_summarize(temp_file, output = "markdown")
    
    # Test result structure
    expect_true(is.list(result))
    expect_true("summary" %in% names(result))
    expect_true("files" %in% names(result))
    expect_true("focus" %in% names(result))
    expect_true("provider" %in% names(result))
    
    # Test contents
    expect_equal(result$files, temp_file)
    expect_equal(result$focus, "general")
    expect_true(is.character(result$summary))
    expect_true(grepl("Script Summary", result$summary))
  })
  
  # Clean up
  unlink(temp_file)
})