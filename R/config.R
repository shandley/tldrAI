#' Configure tldrAI settings
#'
#' @param api_key Character string containing the API key for Claude
#' @param openai_api_key Character string containing the API key for OpenAI
#' @param provider Character string specifying the default provider ("claude" or "openai")
#' @param model Character string specifying which Claude model to use
#' @param openai_model Character string specifying which OpenAI model to use
#' @param cache_enabled Logical indicating whether to cache responses
#' @param cache_dir Character string specifying the cache directory
#' @param cache_ttl Numeric value indicating cache time-to-live in days
#' @param offline_mode Logical indicating whether to operate in offline mode (use cache only)
#' @param enable_fallback Logical indicating whether to enable provider fallback
#' @param fallback_provider Character string specifying the fallback provider
#' @param verbose_default Logical indicating the default verbosity
#' @param examples_default Integer indicating the default number of examples
#' @param character_voice Character string specifying the default character voice
#' @param show_progress Logical indicating whether to show a progress bar during API calls
#'
#' @return Invisibly returns the updated configuration
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_config(api_key = "your_claude_api_key")
#' tldr_config(openai_api_key = "your_openai_api_key", provider = "openai")
#' tldr_config(enable_fallback = TRUE, fallback_provider = "openai")
#' tldr_config(verbose_default = TRUE)
#' tldr_config(offline_mode = TRUE)  # Use cached responses only
#' tldr_config(character_voice = "enthusiastic_explorer")  # Set default character voice
#' tldr_config(show_progress = TRUE)  # Show progress bar during API calls
#' }
tldr_config <- function(api_key = NULL, openai_api_key = NULL, 
                       provider = NULL, model = NULL, openai_model = NULL,
                       cache_enabled = NULL, cache_dir = NULL, cache_ttl = NULL,
                       offline_mode = NULL, enable_fallback = NULL, fallback_provider = NULL,
                       verbose_default = NULL, examples_default = NULL, character_voice = NULL,
                       show_progress = NULL) {
  
  config <- get_config_all()
  
  # Update config with non-NULL values
  if (!is.null(api_key)) config$api_key <- api_key
  if (!is.null(openai_api_key)) config$openai_api_key <- openai_api_key
  if (!is.null(provider)) {
    if (!provider %in% c("claude", "openai")) {
      stop(
        "\n\n╭────────────────────── Invalid Provider Specified ────────────────────╮\n",
        "│                                                                       │\n",
        "│ The provider '", provider, "' is not supported.                             \n",
        "│                                                                       │\n",
        "│ Supported providers are:                                              │\n",
        "│ - \"claude\"  (default)                                                 │\n",
        "│ - \"openai\"                                                            │\n",
        "│                                                                       │\n",
        "│ Example usage:                                                        │\n",
        "│ tldr_config(provider = \"claude\")                                      │\n",
        "│ tldr_config(provider = \"openai\")                                      │\n",
        "│                                                                       │\n",
        "╰───────────────────────────────────────────────────────────────────────╯"
      )
    }
    config$provider <- provider
  }
  if (!is.null(model)) config$model <- model
  if (!is.null(openai_model)) config$openai_model <- openai_model
  if (!is.null(cache_enabled)) config$cache_enabled <- cache_enabled
  if (!is.null(cache_dir)) config$cache_dir <- cache_dir
  if (!is.null(cache_ttl)) {
    if (!is.numeric(cache_ttl) || cache_ttl <= 0) {
      stop("cache_ttl must be a positive number")
    }
    config$cache_ttl <- cache_ttl
  }
  if (!is.null(offline_mode)) config$offline_mode <- offline_mode
  if (!is.null(enable_fallback)) config$enable_fallback <- enable_fallback
  if (!is.null(fallback_provider)) {
    if (!fallback_provider %in% c("claude", "openai")) {
      stop(
        "\n\n╭────────────────── Invalid Fallback Provider Specified ────────────────╮\n",
        "│                                                                       │\n",
        "│ The fallback provider '", fallback_provider, "' is not supported.           \n",
        "│                                                                       │\n",
        "│ Supported providers are:                                              │\n",
        "│ - \"claude\"                                                            │\n",
        "│ - \"openai\"                                                            │\n",
        "│                                                                       │\n",
        "│ Example usage:                                                        │\n",
        "│ tldr_config(enable_fallback = TRUE, fallback_provider = \"openai\")     │\n",
        "│                                                                       │\n",
        "╰───────────────────────────────────────────────────────────────────────╯"
      )
    }
    if (fallback_provider == config$provider) {
      warning("Fallback provider is the same as primary provider. This may not be useful.")
    }
    config$fallback_provider <- fallback_provider
  }
  if (!is.null(verbose_default)) config$verbose_default <- verbose_default
  if (!is.null(examples_default)) config$examples_default <- examples_default
  if (!is.null(character_voice)) {
    # Validate character voice if factory is available
    if (exists("CharacterVoiceFactory")) {
      factory <- CharacterVoiceFactory$new()
      if (!character_voice %in% names(factory$voices)) {
        warning("Character voice '", character_voice, "' not found. Use tldr_list_voices() to see available voices.")
      }
    }
    config$character_voice <- character_voice
  }
  if (!is.null(show_progress)) {
    if (!is.logical(show_progress)) {
      stop("show_progress must be a logical value (TRUE or FALSE)")
    }
    config$show_progress <- show_progress
  }
  
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
      # API keys
      api_key = Sys.getenv("CLAUDE_API_KEY", ""),
      openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
      
      # Provider settings
      provider = "claude",
      fallback_provider = "openai",
      enable_fallback = FALSE,
      
      # Model settings
      model = "claude-3-opus-20240229",
      openai_model = "gpt-4-turbo",
      
      # Cache settings
      cache_enabled = TRUE,
      cache_dir = file.path(rappdirs::user_cache_dir("tldrAI"), "cache"),
      cache_ttl = 30,  # Cache TTL in days
      offline_mode = FALSE,
      
      # Usage settings
      verbose_default = FALSE,
      examples_default = 2,
      
      # Character voice settings
      character_voice = "none",  # Default to no character voice
      
      # UI settings
      show_progress = TRUE  # Show progress bar by default
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
#' @param expired_only Logical indicating whether to clear only expired cache entries
#'
#' @return Invisibly returns TRUE if cache was cleared
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_cache_clear()
#' tldr_cache_clear(expired_only = TRUE)
#' }
tldr_cache_clear <- function(confirm = TRUE, expired_only = FALSE) {
  cache_dir <- get_config("cache_dir")
  
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist.")
    return(invisible(FALSE))
  }
  
  if (expired_only) {
    if (confirm) {
      response <- utils::menu(c("Yes", "No"), 
                        title = paste0("Are you sure you want to clear expired cache entries at '", 
                                      cache_dir, "'?"))
      if (response != 1) {
        return(invisible(FALSE))
      }
    }
    
    # Get cache TTL in days
    cache_ttl <- get_config("cache_ttl", default = 30)
    
    # Get all cache files
    files <- list.files(cache_dir, full.names = TRUE)
    
    if (length(files) == 0) {
      message("Cache is empty.")
      return(invisible(TRUE))
    }
    
    # Check each file's modification time
    now <- Sys.time()
    expired_files <- character(0)
    
    for (file in files) {
      file_time <- file.info(file)$mtime
      if (difftime(now, file_time, units = "days") > cache_ttl) {
        expired_files <- c(expired_files, file)
      }
    }
    
    if (length(expired_files) == 0) {
      message("No expired cache entries found.")
      return(invisible(TRUE))
    }
    
    # Delete expired files
    unlink(expired_files)
    message(paste0(length(expired_files), " expired cache entries cleared."))
    
  } else {
    # Clear the entire cache
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
  }
  
  invisible(TRUE)
}

#' Get the path to the cache file for a function
#'
#' @param func_name The name of the function
#' @param provider The provider name (for provider-specific caching)
#'
#' @return Character string with the path to the cache file
#' @keywords internal
get_cache_path <- function(func_name, provider = NULL) {
  cache_dir <- get_config("cache_dir")
  
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Use different cache files for different providers
  if (!is.null(provider)) {
    cache_key <- paste0(func_name, "_", provider)
  } else {
    cache_key <- func_name
  }
  
  file.path(cache_dir, paste0(cache_key, ".rds"))
}

#' Toggle offline mode
#'
#' @param enable Logical indicating whether to enable (TRUE) or disable (FALSE) offline mode
#'
#' @return Invisibly returns the updated configuration
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_offline(TRUE)  # Enable offline mode
#' tldr_offline(FALSE) # Disable offline mode
#' }
tldr_offline <- function(enable = TRUE) {
  tldr_config(offline_mode = enable)
  
  if (enable) {
    message("Offline mode enabled. Only cached responses will be used.")
  } else {
    message("Offline mode disabled. API calls will be made when needed.")
  }
  
  invisible(get_config_all())
}

#' Test provider connection
#'
#' @param provider The provider to test ("claude" or "openai")
#'
#' @return Logical indicating whether the connection was successful
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_test_connection("claude")
#' tldr_test_connection("openai")
#' }
tldr_test_connection <- function(provider = NULL) {
  if (is.null(provider)) {
    provider <- get_config("provider", default = "claude")
  }
  
  if (!provider %in% c("claude", "openai")) {
    stop("Provider must be one of: claude, openai")
  }
  
  config <- get_config_all()
  factory <- LLMProviderFactory$new()
  
  tryCatch({
    provider_instance <- factory$create_provider(provider, config)
    result <- provider_instance$check_auth()
    
    if (result) {
      message("Successfully connected to ", provider, " API.")
    } else {
      message("Failed to connect to ", provider, " API. Check your API key and internet connection.")
    }
    
    return(invisible(result))
  }, error = function(e) {
    message("Error testing connection to ", provider, " API: ", e$message)
    return(invisible(FALSE))
  })
}