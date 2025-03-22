test_that("multimodal configuration works", {
  # Save current config
  old_config <- get_config_all()
  on.exit({
    # Restore original config
    tldr_config(config = old_config)
  })
  
  # Test setting multimodal config
  result <- tldr_multimodal_config(
    enable_multimodal = TRUE,
    theme = "dark",
    default_height = 500
  )
  
  # Verify config was updated
  config <- get_config_all()
  expect_true(config$multimodal_settings$enable_multimodal)
  expect_equal(config$multimodal_settings$theme, "dark")
  expect_equal(config$multimodal_settings$default_height, 500)
  
  # Test invalid inputs
  expect_warning(tldr_multimodal_config(theme = "invalid"))
  expect_warning(tldr_multimodal_config(default_width = "not_numeric"))
  expect_warning(tldr_multimodal_config(default_visualizations = "invalid"))
  expect_warning(tldr_multimodal_config(custom_colors = "not_a_list"))
})

test_that("multimodal interface can be created", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("htmltools")
  
  # Create a simple test example
  metadata <- list(
    name = "mean",
    package = "base",
    args = c("x", "trim", "na.rm"),
    description = "Arithmetic mean",
    returns = "numeric value"
  )
  
  response_text <- "# mean\n\nCalculates the arithmetic mean."
  
  # Create multimodal interface
  widget <- tldr_multimodal(
    func_name = "mean",
    metadata = metadata,
    response_text = response_text
  )
  
  # Basic checks
  expect_true(inherits(widget, "htmlwidget"))
  expect_equal(widget$name, "tldrAI_multimodal")
  expect_equal(widget$x$func_name, "mean")
})

test_that("helper functions for panels work", {
  skip_if_not_installed("htmltools")
  
  # Test function diagram panel
  metadata <- list(
    name = "mean",
    package = "base",
    args = c("x", "trim", "na.rm"),
    returns = "numeric value"
  )
  
  # Generate a data transformation panel with fallback
  result <- generate_data_transformation_panel("mean", metadata)
  
  # Should return something (either real visualization or fallback)
  expect_true(!is.null(result))
})