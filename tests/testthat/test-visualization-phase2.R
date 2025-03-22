test_that("New visualization types are supported", {
  handler <- VisualizationHandler$new()
  
  # Check that new types are in the supported list
  expect_true("data_flow" %in% handler$supported_vis_types)
  expect_true("function_network" %in% handler$supported_vis_types)
  expect_true("code_highlight" %in% handler$supported_vis_types)
})

test_that("Data flow visualization can be created", {
  # Skip if DiagrammeR is not available
  skip_if_not_installed("DiagrammeR")
  
  # Create a minimal metadata object for a dplyr function
  metadata <- list(
    name = "filter",
    package = "dplyr",
    args = c("data", "filter_expression"),
    signature = "filter(data, ...)"
  )
  
  # Create a visualization
  vis <- create_visualization("filter", metadata, "data_flow")
  
  # Check that it returns a VisualizationHandler object
  expect_true(inherits(vis, "VisualizationHandler"))
  
  # Check that it generated the right visualization type
  expect_equal(vis$visualization_data$vis_type, "data_flow")
})

test_that("ASCII fallbacks are available for new visualization types", {
  # Create metadata for testing
  metadata <- list(
    name = "test_function",
    package = "dplyr",
    args = c("data", "expr")
  )
  
  # Mock requireNamespace to always return FALSE
  mockr::with_mock(
    requireNamespace = function(...) FALSE,
    {
      # Create handler
      handler <- VisualizationHandler$new()
      
      # Generate data flow visualization
      vis_data_flow <- handler$generate_visualization("test_function", metadata, "data_flow", prompt_install = FALSE)
      expect_true(is.character(vis_data_flow))
      expect_true(grepl("Data Flow Diagram", vis_data_flow))
      
      # Generate function network visualization
      vis_network <- handler$generate_visualization("test_function", metadata, "function_network", prompt_install = FALSE)
      expect_true(is.character(vis_network))
      expect_true(grepl("Function Network", vis_network))
      
      # Generate code highlight visualization
      vis_code <- handler$generate_visualization("test_function", metadata, "code_highlight", prompt_install = FALSE)
      expect_true(is.character(vis_code))
      expect_true(grepl("Code Highlight", vis_code))
    }
  )
})

test_that("Export functionality works correctly", {
  # Skip if file writing permission issues
  temp_dir <- tempdir()
  skip_if_not(dir.exists(temp_dir) && file.access(temp_dir, 2) == 0, "Cannot write to temp directory")
  
  # Create a minimal metadata object
  metadata <- list(
    name = "test_function",
    package = "base",
    args = c("x", "y", "z")
  )
  
  # Create a visualization with ASCII fallback (no packages needed)
  mockr::with_mock(
    requireNamespace = function(...) FALSE,
    {
      vis <- create_visualization("test_function", metadata, "diagram", prompt_install = FALSE)
      
      # Test exporting to text file
      txt_path <- file.path(temp_dir, "test_vis.txt")
      export_result <- tryCatch({
        export_visualization(vis, txt_path, format = "txt")
        TRUE
      }, error = function(e) FALSE)
      
      expect_true(export_result)
      expect_true(file.exists(txt_path))
      
      # Clean up
      if (file.exists(txt_path)) {
        unlink(txt_path)
      }
    }
  )
})

test_that("Interactive elements are properly flagged", {
  # Create a mock visualization with interactivity
  metadata <- list(
    name = "test_function",
    package = "base",
    args = c("x", "y")
  )
  
  handler <- VisualizationHandler$new()
  
  # Before generating visualization, interactive should be FALSE
  expect_false(handler$visualization_data$interactive)
  
  # Mock an interactive visualization generation
  mockr::with_mock(
    # Mock generation to set interactive flag
    VisualizationHandler$generate_data_flow_diagram = function(...) {
      self$visualization_data$interactive <- TRUE
      return("Mock interactive visualization")
    },
    requireNamespace = function(...) TRUE,
    {
      # Generate visualization
      handler$generate_visualization("test_function", metadata, "data_flow")
      
      # Check that interactivity flag is set
      expect_true(handler$visualization_data$interactive)
    }
  )
})