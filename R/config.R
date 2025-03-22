#' Configure tldrAI settings
#'
#' @param api_key Character string containing the API key for the LLM service
#' @param model Character string specifying which model to use
#' @param cache_enabled Logical indicating whether to cache responses
#' @param cache_dir Character string specifying the cache directory
#' @param verbose_default Logical indicating the default verbosity
#' @param examples_default Integer indicating the default number of examples
#'
#' @return Invisibly returns the updated configuration
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_config(api_key = "your_api_key")
#' tldr_config(verbose_default = TRUE)
#' }
tldr_config <- function(api_key = NULL, model = NULL, 
                       cache_enabled = NULL, cache_dir = NULL,
                       verbose_default = NULL, examples_default = NULL) {
  
  config <- get_config_all()
  
  # Update config with non-NULL values
  if (!is.null(api_key)) config$api_key <- api_key
  if (!is.null(model)) config$model <- model
  if (!is.null(cache_enabled)) config$cache_enabled <- cache_enabled
  if (!is.null(cache_dir)) config$cache_dir <- cache_dir
  if (!is.null(verbose_default)) config$verbose_default <- verbose_default
  if (!is.null(examples_default)) config$examples_default <- examples_default
  
  # Save the updated config
  save_config(config)
  
  invisible(config)
}

#' Get a configuration value
#'
#' @param key The configuration key to retrieve
#' @param default The default value if the key is not found
#'
#' @return The configuration value
#' @keywords internal
get_config <- function(key, default = NULL) {
  config <- get_config_all()
  if (key %in% names(config)) {
    return(config[[key]])
  }
  default
}

#' Get all configuration values
#'
#' @return A list of all configuration values
#' @keywords internal
get_config_all <- function() {
  config_path <- get_config_path()
  
  if (file.exists(config_path)) {
    config <- readRDS(config_path)
  } else {
    # Default configuration
    config <- list(
      api_key = Sys.getenv("CLAUDE_API_KEY", ""),
      model = "claude-3-opus-20240229",
      cache_enabled = TRUE,
      cache_dir = file.path(rappdirs::user_cache_dir("tldrAI"), "cache"),
      verbose_default = FALSE,
      examples_default = 2
    )
    
    # Create directory if it doesn't exist
    if (!dir.exists(dirname(config_path))) {
      dir.create(dirname(config_path), recursive = TRUE)
    }
    
    save_config(config)
  }
  
  config
}

#' Save the configuration
#'
#' @param config The configuration list to save
#'
#' @return Invisibly returns the config
#' @keywords internal
save_config <- function(config) {
  config_path <- get_config_path()
  
  # Create directory if it doesn't exist
  if (!dir.exists(dirname(config_path))) {
    dir.create(dirname(config_path), recursive = TRUE)
  }
  
  saveRDS(config, config_path)
  invisible(config)
}

#' Get the path to the configuration file
#'
#' @return Character string with the path to the configuration file
#' @keywords internal
get_config_path <- function() {
  file.path(rappdirs::user_config_dir("tldrAI"), "config.rds")
}

#' Clear the cache
#'
#' @param confirm Logical indicating whether to ask for confirmation
#'
#' @return Invisibly returns TRUE if cache was cleared
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_cache_clear()
#' }
tldr_cache_clear <- function(confirm = TRUE) {
  cache_dir <- get_config("cache_dir")
  
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist.")
    return(invisible(FALSE))
  }
  
  if (confirm) {
    response <- utils::menu(c("Yes", "No"), 
                      title = paste0("Are you sure you want to clear the cache at '", 
                                    cache_dir, "'?"))
    if (response != 1) {
      return(invisible(FALSE))
    }
  }
  
  # Delete all files in cache directory
  files <- list.files(cache_dir, full.names = TRUE)
  if (length(files) > 0) {
    unlink(files)
    message("Cache cleared.")
  } else {
    message("Cache is already empty.")
  }
  
  invisible(TRUE)
}

#' Get the path to the cache file for a function
#'
#' @param func_name The name of the function
#'
#' @return Character string with the path to the cache file
#' @keywords internal
get_cache_path <- function(func_name) {
  cache_dir <- get_config("cache_dir")
  
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  file.path(cache_dir, paste0(func_name, ".rds"))
}