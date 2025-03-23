#' Configure multimodal visualization settings
#'
#' @description
#' Configure settings for the multimodal visualization interface, which combines
#' text documentation and interactive visualizations in a two-panel layout.
#'
#' @param enable_multimodal Logical indicating whether to enable multimodal visualization by default
#' @param theme Character string specifying the default theme ("light" or "dark")
#' @param default_width Integer specifying the default width in pixels, or NULL for automatic width
#' @param default_height Integer specifying the default height in pixels
#' @param default_visualizations Character vector specifying which visualization types to include by default
#' @param custom_colors List with custom color values for creating a custom theme
#'
#' @return Invisibly returns the current multimodal visualization settings
#' @export
#'
#' @examples
#' \dontrun{
#' # Enable multimodal visualization by default
#' tldr_multimodal_config(enable_multimodal = TRUE)
#'
#' # Set dark theme as default
#' tldr_multimodal_config(theme = "dark")
#'
#' # Customize height
#' tldr_multimodal_config(default_height = 800)
#'
#' # Specify which visualizations to show
#' tldr_multimodal_config(default_visualizations = c("diagram", "data_flow"))
#'
#' # Create a custom theme with custom colors
#' tldr_multimodal_config(custom_colors = list(
#'   primary = "#1E88E5",     # Primary color (headers, highlights)
#'   secondary = "#43A047",   # Secondary color (sub-headers)
#'   accent = "#FDD835",      # Accent color (active tabs, buttons)
#'   background = "#FFFFFF"   # Background color
#' ))
#' }
tldr_multimodal_config <- function(
  enable_multimodal = NULL,
  theme = NULL,
  default_width = NULL,
  default_height = NULL,
  default_visualizations = NULL,
  custom_colors = NULL
) {
  # Get current settings
  config <- get_config_all()
  
  # Initialize multimodal settings if they don't exist
  if (is.null(config$multimodal_settings)) {
    config$multimodal_settings <- list(
      enable_multimodal = FALSE,
      theme = "light",
      default_width = NULL,
      default_height = 600,
      default_visualizations = c("diagram", "data_flow", "function_network", "code_highlight"),
      custom_colors = NULL
    )
  }
  
  # Update provided settings
  if (!is.null(enable_multimodal)) {
    config$multimodal_settings$enable_multimodal <- enable_multimodal
  }
  
  if (!is.null(theme)) {
    if (!(theme %in% c("light", "dark") || is.list(theme))) {
      warning("Theme must be 'light', 'dark', or a list of custom colors. Using 'light' theme.")
      theme <- "light"
    }
    config$multimodal_settings$theme <- theme
  }
  
  if (!is.null(default_width)) {
    if (!is.numeric(default_width) && !is.null(default_width)) {
      warning("default_width must be numeric or NULL. Using NULL (auto width).")
      default_width <- NULL
    }
    config$multimodal_settings$default_width <- default_width
  }
  
  if (!is.null(default_height)) {
    if (!is.numeric(default_height)) {
      warning("default_height must be numeric. Using 600.")
      default_height <- 600
    }
    config$multimodal_settings$default_height <- default_height
  }
  
  if (!is.null(default_visualizations)) {
    if (!is.character(default_visualizations)) {
      warning("default_visualizations must be a character vector. Using defaults.")
    } else {
      supported_types <- c("diagram", "flowchart", "data_flow", "function_network", "code_highlight")
      valid_types <- default_visualizations[default_visualizations %in% supported_types]
      
      if (length(valid_types) == 0) {
        warning("No valid visualization types provided. Using defaults.")
      } else {
        config$multimodal_settings$default_visualizations <- valid_types
      }
    }
  }
  
  if (!is.null(custom_colors)) {
    if (!is.list(custom_colors)) {
      warning("custom_colors must be a list. Ignoring custom colors.")
    } else {
      config$multimodal_settings$custom_colors <- custom_colors
    }
  }
  
  # Save updated config
  tldr_config(config = config)
  
  message("Multimodal visualization settings updated.")
  invisible(config$multimodal_settings)
}