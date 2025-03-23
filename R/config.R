#' Configure tldrAI settings
#'
#' This function allows you to configure various settings for the tldrAI package,
#' including API keys, provider preferences, caching behavior, default display
#' options, performance settings, and visualization settings. You can call it
#' with one or more parameters to update specific settings without affecting others.
#'
#' @section API Settings:
#' \describe{
#'   \item{api_key}{Character string containing the API key for Claude (Anthropic).
#'        Required if using the Claude provider.}
#'   \item{openai_api_key}{Character string containing the API key for OpenAI.
#'        Required if using the OpenAI provider.}
#'   \item{provider}{Character string specifying the default provider.
#'        Options: "claude" (default) or "openai".}
#'   \item{model}{Character string specifying which Claude model to use.
#'        Options: "claude-3-opus-20240229" (default, most capable),
#'        "claude-3-sonnet-20240229" (balanced), "claude-3-haiku-20240307" (fastest),
#'        or "claude-2.1" (legacy).}
#'   \item{openai_model}{Character string specifying which OpenAI model to use.
#'        Options: "gpt-4o" (latest), "gpt-4-turbo" (default), "gpt-4" (legacy),
#'        or "gpt-3.5-turbo" (faster, less capable).}
#'   \item{use_keyring}{Logical indicating whether to use the system keyring for API key storage.
#'        Default: TRUE. When TRUE, API keys are stored in the secure system keyring if the
#'        keyring package is available. Use \code{\link{tldr_set_api_key}} for better control.}
#' }
#'
#' @section Caching Settings:
#' \describe{
#'   \item{cache_enabled}{Logical indicating whether to cache responses.
#'        Default: TRUE. Caching improves performance and reduces API costs.}
#'   \item{cache_dir}{Character string specifying the cache directory.
#'        Default: Uses rappdirs::user_cache_dir("tldrAI").}
#'   \item{cache_ttl}{Numeric value indicating cache time-to-live in days.
#'        Default: 30. After this period, cached responses are considered expired.}
#'   \item{offline_mode}{Logical indicating whether to operate in offline mode (use cache only).
#'        Default: FALSE. When TRUE, no API calls will be made, only cached responses are used.}
#' }
#'
#' @section Fallback Settings:
#' \describe{
#'   \item{enable_fallback}{Logical indicating whether to enable provider fallback.
#'        Default: FALSE. When TRUE, automatically tries the fallback provider if the primary fails.}
#'   \item{fallback_provider}{Character string specifying the fallback provider.
#'        Default: "openai" if primary is "claude", and vice versa.}
#' }
#'
#' @section Display Settings:
#' \describe{
#'   \item{verbose_default}{Logical indicating the default verbosity.
#'        Default: FALSE. When TRUE, more detailed information is shown.}
#'   \item{examples_default}{Integer indicating the default number of examples.
#'        Default: 2. Higher values provide more diverse examples.}
#'   \item{character_voice}{Character string specifying the default character voice.
#'        Default: "none". Use tldr_list_voices() to see all options.}
#'   \item{show_progress}{Logical indicating whether to show a progress bar during API calls.
#'        Default: TRUE. Provides feedback while waiting for API responses.}
#' }
#'
#' @section Debug Settings:
#' \describe{
#'   \item{debug_mode}{Logical indicating whether to show debug information.
#'        Default: FALSE. When TRUE, shows detailed diagnostic information.}
#' }
#'
#' @section Performance Settings:
#' \describe{
#'   \item{async_mode}{Logical indicating whether to use asynchronous API calls.
#'        Default: FALSE. When TRUE, enables non-blocking API requests.}
#'   \item{timeout}{Numeric value specifying the API request timeout in seconds.
#'        Default: 60. Increase for slow connections.}
#'   \item{max_retries}{Numeric value specifying the maximum number of retries for API calls.
#'        Default: 3. Increases resilience on unstable connections.}
#' }
#'
#' @section Visualization Settings:
#' \describe{
#'   \item{visualize}{Logical indicating whether to enable visualizations by default.
#'        Default: FALSE. When TRUE, visualizations are included with responses.}
#'   \item{visualization_type}{Character string specifying the default visualization type.
#'        Options: "diagram" (default), "flowchart", "data_flow", "function_network", or "code_highlight".}
#' }
#'
#' @return Invisibly returns the updated configuration, which can be used programmatically
#'         if needed. The function primarily has side effects, updating the package configuration.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # API Configuration
#' tldr_config(api_key = "your_claude_api_key")  # Set Claude API key
#' tldr_config(openai_api_key = "your_openai_api_key")  # Set OpenAI API key
#' tldr_config(provider = "claude")  # Use Claude as default provider
#' tldr_config(model = "claude-3-sonnet-20240229")  # Use a different Claude model
#' tldr_config(openai_model = "gpt-4o")  # Use a different OpenAI model
#' tldr_config(use_keyring = TRUE)  # Enable secure keyring storage for API keys
#' 
#' # Fallback Configuration
#' tldr_config(enable_fallback = TRUE, fallback_provider = "openai")  # Auto-fallback to OpenAI
#' 
#' # Display Configuration
#' tldr_config(verbose_default = TRUE)  # Always show detailed information
#' tldr_config(examples_default = 4)  # Show more examples by default
#' tldr_config(character_voice = "enthusiastic_explorer")  # Set default voice
#' tldr_config(show_progress = FALSE)  # Hide progress messages
#' 
#' # Cache Configuration
#' tldr_config(cache_enabled = FALSE)  # Disable caching
#' tldr_config(cache_ttl = 90)  # Keep cache for 90 days
#' tldr_config(cache_dir = "~/R/tldrAI_cache")  # Custom cache location
#' tldr_config(offline_mode = TRUE)  # Work in offline mode
#' 
#' # Performance Configuration
#' tldr_config(async_mode = TRUE)  # Enable asynchronous API calls
#' tldr_config(timeout = 120)  # Longer timeout for slow connections
#' tldr_config(max_retries = 5)  # More retries for unreliable networks
#' 
#' # Visualization Configuration
#' tldr_config(visualize = TRUE)  # Always include visualizations
#' tldr_config(visualization_type = "function_network")  # Set default visualization type
#' 
#' # Multiple settings at once
#' tldr_config(
#'   provider = "claude",
#'   character_voice = "wise_mentor",
#'   verbose_default = TRUE,
#'   visualize = TRUE
#' )
#' }
tldr_config <- function(api_key = NULL, openai_api_key = NULL, 
                       provider = NULL, model = NULL, openai_model = NULL,
                       cache_enabled = NULL, cache_dir = NULL, cache_ttl = NULL,
                       offline_mode = NULL, enable_fallback = NULL, fallback_provider = NULL,
                       verbose_default = NULL, examples_default = NULL, character_voice = NULL,
                       show_progress = NULL, debug_mode = NULL, async_mode = NULL,
                       timeout = NULL, max_retries = NULL, visualize = NULL,
                       visualization_type = NULL, use_keyring = NULL) {
  
  config <- get_config_all()
  
  # Update config with non-NULL values
  if (!is.null(api_key)) {
    # Check if we should store the key in keyring
    if (requireNamespace("keyring", quietly = TRUE) && 
        get_config("use_keyring", default = TRUE)) {
      tldr_set_api_key(api_key, "claude", update_config = TRUE)
      message("Claude API key stored securely in system keyring.")
    } else {
      config$api_key <- api_key
    }
  }
  
  if (!is.null(openai_api_key)) {
    # Check if we should store the key in keyring
    if (requireNamespace("keyring", quietly = TRUE) && 
        get_config("use_keyring", default = TRUE)) {
      tldr_set_api_key(openai_api_key, "openai", update_config = TRUE)
      message("OpenAI API key stored securely in system keyring.")
    } else {
      config$openai_api_key <- openai_api_key
    }
  }
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
  
  if (!is.null(debug_mode)) {
    if (!is.logical(debug_mode)) {
      stop("debug_mode must be a logical value (TRUE or FALSE)")
    }
    config$debug_mode <- debug_mode
  }
  
  if (!is.null(async_mode)) {
    if (!is.logical(async_mode)) {
      stop("async_mode must be a logical value (TRUE or FALSE)")
    }
    if (async_mode && !requireNamespace("future", quietly = TRUE)) {
      warning("The 'future' package is not installed. Async mode requires the 'future' package. Install with: install.packages(\"future\")")
      config$async_mode <- FALSE
    } else if (async_mode && !requireNamespace("promises", quietly = TRUE)) {
      warning("The 'promises' package is not installed. Async mode requires the 'promises' package. Install with: install.packages(\"promises\")")
      config$async_mode <- FALSE
    } else {
      config$async_mode <- async_mode
    }
  }
  
  if (!is.null(timeout)) {
    if (!is.numeric(timeout) || timeout <= 0) {
      stop("timeout must be a positive number")
    }
    config$timeout <- timeout
  }
  
  if (!is.null(max_retries)) {
    if (!is.numeric(max_retries) || max_retries < 0 || max_retries != as.integer(max_retries)) {
      stop("max_retries must be a non-negative integer")
    }
    config$max_retries <- as.integer(max_retries)
  }
  
  # Handle use_keyring option
  if (!is.null(use_keyring)) {
    if (!is.logical(use_keyring)) {
      stop("use_keyring must be a logical value (TRUE or FALSE)")
    }
    
    if (use_keyring && !requireNamespace("keyring", quietly = TRUE)) {
      warning("The keyring package is not installed. Install it with: install.packages(\"keyring\")")
      config$use_keyring <- FALSE
    } else {
      config$use_keyring <- use_keyring
      
      # If enabling keyring, suggest migrating existing keys
      if (use_keyring && requireNamespace("keyring", quietly = TRUE) &&
          ((!is.null(config$api_key) && nchar(config$api_key) > 0) ||
           (!is.null(config$openai_api_key) && nchar(config$openai_api_key) > 0))) {
        message("You have API keys stored in configuration that could be moved to keyring.")
        message("Run tldr_key_migrate() to securely store your existing API keys.")
      }
    }
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
      
      # Security settings
      use_keyring = TRUE,  # Use keyring for API key storage if available
      
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
      refresh_mode = FALSE,  # Whether to ignore cached content
      
      # Usage settings
      verbose_default = FALSE,
      examples_default = 2,
      
      # Character voice settings
      character_voice = "none",  # Default to no character voice
      
      # UI settings
      show_progress = TRUE,  # Show progress bar by default
      debug_mode = FALSE,  # Debug mode
      
      # Performance settings
      async_mode = FALSE,  # Asynchronous API calls
      timeout = 60,  # API request timeout in seconds
      max_retries = 3,  # Maximum number of retries for API calls
      batch_size = 1,  # Number of concurrent requests
      
      # Visualization settings
      visualization_settings = list(
        enable_visualization = FALSE,  # Enable visualizations
        default_type = "diagram",  # Default visualization type
        auto_install = FALSE,  # Automatically install required packages
        auto_export = FALSE,  # Automatically export visualizations
        export_format = "svg",  # Default export format
        export_dir = getwd()  # Default export directory
      )
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
  
  # Make the cache key more unique by including package information if available
  pkg <- find_package(func_name)
  if (!is.null(pkg) && pkg != "unknown") {
    cache_key <- paste0(pkg, "__", cache_key)
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

#' Toggle debug mode
#'
#' @param enable Logical indicating whether to enable (TRUE) or disable (FALSE) debug mode
#'
#' @return Invisibly returns the updated configuration
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_debug(TRUE)  # Enable debug mode
#' tldr_debug(FALSE) # Disable debug mode
#' }
tldr_debug <- function(enable = TRUE) {
  tldr_config(debug_mode = enable)
  
  if (enable) {
    message("Debug mode enabled. Additional diagnostic information will be shown.")
  } else {
    message("Debug mode disabled.")
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