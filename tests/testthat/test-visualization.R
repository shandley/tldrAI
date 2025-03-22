test_that("VisualizationHandler can be initialized", {
  # Create a handler with default settings
  handler <- VisualizationHandler$new()
  
  # Check that the handler contains expected fields
  expect_true(is.list(handler$visualization_data))
  expect_true(is.character(handler$supported_vis_types))
  expect_true("diagram" %in% handler$supported_vis_types)
  expect_true("flowchart" %in% handler$supported_vis_types)
})

test_that("create_visualization returns a visualization handler", {
  # Create a minimal metadata object
  metadata <- list(
    name = "test_function",
    package = "base",
    args = c("x", "y", "z"),
    signature = "test_function(x, y, z)"
  )
  
  # Create a visualization
  vis <- create_visualization("test_function", metadata, "diagram")
  
  # Check that it returns a VisualizationHandler object
  expect_true(inherits(vis, "VisualizationHandler"))
})

test_that("visualization configuration can be updated", {
  # Save current config to restore later
  original_config <- get_config_all()
  
  # Update visualization settings
  tldr_visualization_config(enable = TRUE, default_type = "diagram")
  
  # Get updated config
  config <- get_config_all()
  
  # Check that settings were updated
  expect_true(is.list(config$visualization_settings))
  expect_true(config$visualization_settings$enable_visualization)
  expect_equal(config$visualization_settings$default_type, "diagram")
  
  # Restore original settings
  invisible(save_config(original_config))
})

test_that("check_visualization_packages identifies required packages", {
  # Check required packages for diagram type
  pkg_list <- get_visualization_packages("diagram")
  
  # It should return a character vector
  expect_true(is.character(pkg_list))
  expect_true(length(pkg_list) > 0)
  expect_true("DiagrammeR" %in% pkg_list)
})

test_that("visualization handler gracefully handles missing packages", {
  # Mock requireNamespace to always return FALSE
  mockr::with_mock(
    requireNamespace = function(...) FALSE,
    {
      # Create handler
      handler <- VisualizationHandler$new()
      
      # Check package availability (should be FALSE)
      available <- handler$check_packages("diagram")
      expect_false(available)
      
      # Generate visualization (should return NULL or message)
      metadata <- list(
        name = "test_function",
        package = "base",
        args = c("x", "y", "z")
      )
      
      vis <- handler$generate_visualization("test_function", metadata, "diagram")
      expect_true(is.character(vis) || is.null(vis))
    }
  )
})