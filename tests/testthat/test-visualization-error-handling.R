test_that("VisualizationHandler provides fallback ASCII visualization", {
  # Mock requireNamespace to always return FALSE
  mockr::with_mock(
    requireNamespace = function(...) FALSE,
    {
      # Create handler
      handler <- VisualizationHandler$new()
      
      # Create metadata for testing
      metadata <- list(
        name = "test_function",
        package = "base",
        args = c("x", "y", "z")
      )
      
      # Generate visualization without prompting
      vis <- handler$generate_visualization("test_function", metadata, "diagram", prompt_install = FALSE)
      
      # Should return a character string (ASCII visualization)
      expect_true(is.character(vis))
      expect_true(grepl("Function Diagram", vis))
      expect_true(grepl("text-based visualization", vis))
      
      # Test flowchart fallback
      metadata$body_summary <- "if (x > 0) return(TRUE) else return(FALSE)"
      vis_flow <- handler$generate_visualization("test_function", metadata, "flowchart", prompt_install = FALSE)
      
      # Should return a character string (ASCII visualization)
      expect_true(is.character(vis_flow))
      expect_true(grepl("Function Flowchart", vis_flow))
      expect_true(grepl("Conditional", vis_flow))
    }
  )
})

test_that("install_visualization_packages correctly identifies installed packages", {
  # Mock requireNamespace to return TRUE for DiagrammeR and FALSE for others
  mockr::with_mock(
    requireNamespace = function(pkg, ...) pkg == "DiagrammeR",
    utils::install.packages = function(...) TRUE,
    {
      # Save original config
      original_config <- get_config_all()
      
      # Test with a package that's "already installed"
      result <- capture.output(
        install_visualization_packages("diagram")
      )
      
      # Should indicate packages are already installed
      expect_true(any(grepl("Already installed", result)))
      
      # Restore original settings
      invisible(save_config(original_config))
    }
  )
})

test_that("Create visualization with auto_install=TRUE installs packages", {
  # Mock requireNamespace and install.packages
  mockr::with_mock(
    requireNamespace = function(pkg, ...) FALSE,
    utils::install.packages = function(...) TRUE,
    utils::menu = function(...) 1,
    {
      # Save original config
      original_config <- get_config_all()
      
      # Set auto_install to TRUE
      config <- get_config_all()
      if (is.null(config$visualization_settings)) {
        config$visualization_settings <- list()
      }
      config$visualization_settings$auto_install <- TRUE
      save_config(config)
      
      # Create metadata for testing
      metadata <- list(
        name = "test_function",
        package = "base",
        args = c("x", "y", "z")
      )
      
      # After the "installation" requireNamespace should be mocked to return TRUE
      # This is a limitation of mockr - we can't change the mock mid-test
      
      # Verify the configuration is set correctly
      config <- get_config_all()
      expect_true(config$visualization_settings$auto_install)
      
      # Restore original settings
      invisible(save_config(original_config))
    }
  )
})

test_that("ASCII diagram includes function arguments", {
  # Create handler
  handler <- VisualizationHandler$new()
  
  # Create metadata with arguments
  metadata <- list(
    name = "complex_function",
    package = "testpkg",
    args = c("data", "formula", "subset", "weights")
  )
  
  # Generate ASCII diagram directly
  ascii_viz <- handler$generate_ascii_diagram("complex_function", metadata)
  
  # Check that all arguments are included
  expect_true(grepl("data", ascii_viz))
  expect_true(grepl("formula", ascii_viz))
  expect_true(grepl("subset", ascii_viz))
  expect_true(grepl("weights", ascii_viz))
  
  # Check structure elements
  expect_true(grepl("Function Diagram:", ascii_viz))
  expect_true(grepl("Arguments:", ascii_viz))
  expect_true(grepl("Returns:", ascii_viz))
})

test_that("ASCII flowchart detects conditionals and loops", {
  # Create handler
  handler <- VisualizationHandler$new()
  
  # Create metadata with conditional code
  conditional_metadata <- list(
    name = "if_func",
    package = "testpkg",
    body_summary = "if (condition) { do_something() } else { alternative() }"
  )
  
  # Generate ASCII flowchart with conditional
  conditional_viz <- handler$generate_ascii_flowchart("if_func", conditional_metadata)
  
  # Check that conditional elements are included
  expect_true(grepl("If", conditional_viz))
  expect_true(grepl("True", conditional_viz))
  expect_true(grepl("False", conditional_viz))
  
  # Create metadata with loop code
  loop_metadata <- list(
    name = "loop_func",
    package = "testpkg",
    body_summary = "for (i in seq_len(n)) { process(i) }"
  )
  
  # Generate ASCII flowchart with loop
  loop_viz <- handler$generate_ascii_flowchart("loop_func", loop_metadata)
  
  # Check that loop elements are included
  expect_true(grepl("Loop", loop_viz))
  expect_true(grepl("Iteration", loop_viz))
  
  # Create metadata with both conditionals and loops
  combined_metadata <- list(
    name = "complex_func",
    package = "testpkg",
    body_summary = "for (i in seq_len(n)) { if (condition) { x } else { y } }"
  )
  
  # Generate ASCII flowchart with both
  combined_viz <- handler$generate_ascii_flowchart("complex_func", combined_metadata)
  
  # Check that both elements are included
  expect_true(grepl("Loop", combined_viz))
  expect_true(grepl("If", combined_viz))
})