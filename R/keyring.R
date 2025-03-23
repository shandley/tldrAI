#' Store API key securely in the system keyring
#'
#' This function securely stores an API key in the system's credential store
#' using the keyring package. The key is stored with a service name specific
#' to tldrAI and a username matching the provider name.
#'
#' @param api_key Character string containing the API key to store.
#' @param provider Character string specifying the provider ("claude" or "openai").
#' @param update_config Logical indicating whether to update the configuration.
#'        When TRUE, removes the key from the config file after storing it securely.
#'        Default is TRUE.
#'
#' @return Invisibly returns TRUE if the key was successfully stored.
#'
#' @section Security Benefits:
#' Using the system keyring provides several security advantages:
#' \itemize{
#'   \item Keys are not stored in plain text in configuration files
#'   \item System keyring is typically encrypted
#'   \item Access to stored keys requires system authentication
#'   \item Keys are not visible in R environment or history
#' }
#'
#' @section Fallback Behavior:
#' If the keyring package is not available or fails:
#' \itemize{
#'   \item A warning is displayed
#'   \item The key is stored in the regular configuration (if update_config=TRUE)
#'   \item The function returns FALSE
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Store Claude API key
#' tldr_set_api_key("your_claude_api_key", "claude")
#'
#' # Store OpenAI API key
#' tldr_set_api_key("your_openai_api_key", "openai")
#'
#' # Store key without updating configuration
#' tldr_set_api_key("your_api_key", "claude", update_config = FALSE)
#' }
tldr_set_api_key <- function(api_key, provider = c("claude", "openai"), update_config = TRUE) {
  provider <- match.arg(provider)
  
  # Check if keyring package is available
  if (!requireNamespace("keyring", quietly = TRUE)) {
    warning(
      "The keyring package is not installed. Keys will be stored in configuration file.\n",
      "To use secure key storage, install the keyring package with: install.packages(\"keyring\")"
    )
    
    # Fallback to storing in configuration
    if (update_config) {
      config_param <- if (provider == "claude") "api_key" else "openai_api_key"
      do.call(tldr_config, setNames(list(api_key), config_param))
      message("API key stored in configuration file.")
    }
    
    return(invisible(FALSE))
  }
  
  # Determine keyring settings
  service_name <- "tldrAI"
  username <- provider
  
  tryCatch({
    # Store the key in the keyring
    keyring::key_set_with_value(service = service_name, username = username, password = api_key)
    
    # If successful and update_config is TRUE, remove from config file
    if (update_config) {
      config <- get_config_all()
      
      # Remove key from configuration
      if (provider == "claude") {
        config$api_key <- ""
      } else if (provider == "openai") {
        config$openai_api_key <- ""
      }
      
      # Save updated configuration
      save_config(config)
    }
    
    message("API key for ", provider, " securely stored in system keyring.")
    invisible(TRUE)
    
  }, error = function(e) {
    warning(
      "Failed to store API key in keyring: ", e$message, "\n",
      "Falling back to configuration file storage."
    )
    
    # Fallback to storing in configuration
    if (update_config) {
      config_param <- if (provider == "claude") "api_key" else "openai_api_key"
      do.call(tldr_config, setNames(list(api_key), config_param))
      message("API key stored in configuration file.")
    }
    
    invisible(FALSE)
  })
}

#' Check if API key is available
#'
#' This function checks if an API key is available for the specified provider,
#' either in the system keyring, environment variables, or configuration file.
#'
#' @param provider Character string specifying the provider ("claude" or "openai").
#'
#' @return Logical indicating whether an API key is available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if Claude API key is available
#' tldr_has_api_key("claude")
#'
#' # Check if OpenAI API key is available
#' tldr_has_api_key("openai")
#' }
tldr_has_api_key <- function(provider = c("claude", "openai")) {
  provider <- match.arg(provider)
  
  # Try to get the key without exposing it
  tryCatch({
    key <- get_api_key(provider)
    return(!is.null(key) && nchar(key) > 0)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Clear API key from system keyring
#'
#' This function removes an API key from the system keyring for the specified provider.
#'
#' @param provider Character string specifying the provider ("claude" or "openai").
#' @param confirm Logical indicating whether to ask for confirmation before clearing.
#'        Default is TRUE.
#'
#' @return Invisibly returns TRUE if the key was successfully removed or FALSE if
#'         the operation was cancelled or failed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear Claude API key
#' tldr_clear_api_key("claude")
#'
#' # Clear OpenAI API key without confirmation
#' tldr_clear_api_key("openai", confirm = FALSE)
#' }
tldr_clear_api_key <- function(provider = c("claude", "openai"), confirm = TRUE) {
  provider <- match.arg(provider)
  
  # Check if keyring package is available
  if (!requireNamespace("keyring", quietly = TRUE)) {
    warning(
      "The keyring package is not installed. No keys to clear from keyring.\n",
      "You may still have keys stored in your configuration file.\n",
      "To clear those, use tldr_config(", 
      if (provider == "claude") "api_key = \"\")" else "openai_api_key = \"\")"
    )
    return(invisible(FALSE))
  }
  
  # Determine keyring settings
  service_name <- "tldrAI"
  username <- provider
  
  # Check if the key exists in keyring
  key_exists <- FALSE
  tryCatch({
    keyring::key_list(service = service_name) |>
      subset(username == provider) |>
      nrow() > 0 -> key_exists
  }, error = function(e) {
    key_exists <<- FALSE
  })
  
  if (!key_exists) {
    message("No API key found in keyring for provider: ", provider)
    return(invisible(FALSE))
  }
  
  # Confirm deletion if requested
  if (confirm) {
    response <- utils::menu(c("Yes", "No"), 
                          title = paste0("Are you sure you want to remove the ", provider, " API key from the keyring?"))
    if (response != 1) {
      message("Operation cancelled.")
      return(invisible(FALSE))
    }
  }
  
  # Delete the key
  tryCatch({
    keyring::key_delete(service = service_name, username = username)
    message("API key for ", provider, " has been removed from the system keyring.")
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to remove API key from keyring: ", e$message)
    invisible(FALSE)
  })
}

#' Migrate API keys from configuration to keyring
#'
#' This internal function detects and migrates API keys from the configuration
#' file to the system keyring. It's called automatically during package startup
#' if the keyring package is available.
#'
#' @param ask_confirmation Logical indicating whether to ask for user confirmation
#'        before migrating keys. Default is TRUE.
#'
#' @return Invisibly returns a logical vector indicating which providers had keys migrated.
#'
#' @keywords internal
migrate_api_keys <- function(ask_confirmation = TRUE) {
  # Check if keyring package is available
  if (!requireNamespace("keyring", quietly = TRUE)) {
    return(invisible(c(claude = FALSE, openai = FALSE)))
  }
  
  config <- get_config_all()
  results <- c(claude = FALSE, openai = FALSE)
  
  # Check for Claude key
  if (!is.null(config$api_key) && nchar(config$api_key) > 0) {
    should_migrate <- TRUE
    
    if (ask_confirmation) {
      message("Found Claude API key in configuration file.")
      response <- utils::menu(c("Yes", "No"), 
                            title = "Would you like to move it to the secure system keyring?")
      should_migrate <- response == 1
    }
    
    if (should_migrate) {
      results["claude"] <- tldr_set_api_key(config$api_key, "claude")
    }
  }
  
  # Check for OpenAI key
  if (!is.null(config$openai_api_key) && nchar(config$openai_api_key) > 0) {
    should_migrate <- TRUE
    
    if (ask_confirmation) {
      message("Found OpenAI API key in configuration file.")
      response <- utils::menu(c("Yes", "No"), 
                            title = "Would you like to move it to the secure system keyring?")
      should_migrate <- response == 1
    }
    
    if (should_migrate) {
      results["openai"] <- tldr_set_api_key(config$openai_api_key, "openai")
    }
  }
  
  invisible(results)
}

#' Get API key securely
#'
#' This function retrieves an API key for the specified provider, checking
#' the system keyring, environment variables, and configuration file in that order.
#'
#' @param provider Character string specifying the provider ("claude" or "openai").
#'
#' @return Character string containing the API key, or NULL if no key is found.
#'
#' @keywords internal
get_api_key <- function(provider = c("claude", "openai")) {
  provider <- match.arg(provider)
  
  # First check environment variables
  env_var <- if (provider == "claude") "CLAUDE_API_KEY" else "OPENAI_API_KEY"
  env_key <- Sys.getenv(env_var, "")
  if (nchar(env_key) > 0) {
    return(env_key)
  }
  
  # Then try the keyring if available
  if (requireNamespace("keyring", quietly = TRUE)) {
    service_name <- "tldrAI"
    username <- provider
    
    tryCatch({
      if (any(keyring::key_list(service = service_name)$username == username)) {
        return(keyring::key_get(service = service_name, username = username))
      }
    }, error = function(e) {
      if (get_config("debug_mode", default = FALSE)) {
        warning("Error accessing keyring: ", e$message)
      }
      # Continue to config file
    })
  }
  
  # Finally, fall back to the configuration file
  config <- get_config_all()
  config_key <- if (provider == "claude") config$api_key else config$openai_api_key
  
  if (!is.null(config_key) && nchar(config_key) > 0) {
    return(config_key)
  }
  
  # No key found
  return(NULL)
}

# Add .onLoad function to handle automatic migration
.onLoad <- function(libname, pkgname) {
  # Check if any keys should be migrated, but don't prompt during package load
  # This will happen only in interactive sessions and on first load
  if (interactive() && requireNamespace("keyring", quietly = TRUE)) {
    config <- tryCatch(get_config_all(), error = function(e) NULL)
    
    if (!is.null(config)) {
      has_claude_key <- !is.null(config$api_key) && nchar(config$api_key) > 0
      has_openai_key <- !is.null(config$openai_api_key) && nchar(config$openai_api_key) > 0
      
      if (has_claude_key || has_openai_key) {
        message("tldrAI: Found API keys in configuration that could be stored more securely.")
        message("       Run tldr_key_migrate() to move keys to your system's secure keyring.")
      }
    }
  }
}

#' Migrate API keys to secure storage
#'
#' This function detects any API keys stored in the configuration file and
#' offers to migrate them to the system's secure keyring.
#'
#' @return Invisibly returns a logical vector indicating which providers had keys migrated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Migrate all API keys to secure storage
#' tldr_key_migrate()
#' }
tldr_key_migrate <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    message(
      "The keyring package is required for secure API key storage.\n",
      "Install it with: install.packages(\"keyring\")"
    )
    return(invisible(c(claude = FALSE, openai = FALSE)))
  }
  
  migrate_api_keys(ask_confirmation = TRUE)
}