#' LLM Provider Class
#'
#' @description Base class for LLM API providers
#'
#' @keywords internal
LLMProvider <- R6::R6Class("LLMProvider",
  public = list(
    #' @field provider_name Name of the provider
    provider_name = NULL,
    
    #' @field provider_models Available models
    provider_models = character(),
    
    #' @description Initialize a new LLM provider
    #' @param config Configuration list
    initialize = function(config) {
      stop("This is an abstract class and should not be instantiated directly")
    },
    
    #' @description Get response from the API
    #' @param prompt The prompt to send
    #' @return API response text
    get_response = function(prompt) {
      stop("This method must be implemented by subclasses")
    },
    
    #' @description Check if authentication is valid
    #' @return Logical indicating if auth is valid
    check_auth = function() {
      stop("This method must be implemented by subclasses")
    }
  )
)

#' Claude API Provider
#'
#' @description Provider implementation for Anthropic's Claude API
#'
#' @keywords internal
ClaudeProvider <- R6::R6Class("ClaudeProvider",
  inherit = LLMProvider,
  public = list(
    #' @field api_key API key
    api_key = NULL,
    #' @field model Model name
    model = NULL,
    #' @field max_retries Maximum number of retries
    max_retries = 3,
    
    #' @field timeout API request timeout in seconds
    timeout = 60,
    
    #' @description Initialize a new Claude provider
    #' @param config Configuration list
    initialize = function(config) {
      self$provider_name <- "claude"
      self$provider_models <- c(
        "claude-3-opus-20240229",
        "claude-3-sonnet-20240229",
        "claude-3-haiku-20240307",
        "claude-2.1"
      )
      
      # Set API key, model, and performance settings
      self$api_key <- config$api_key
      self$model <- config$model
      
      # Set custom timeout and max_retries if provided
      if (!is.null(config$timeout) && is.numeric(config$timeout) && config$timeout > 0) {
        self$timeout <- config$timeout
      }
      
      if (!is.null(config$max_retries) && is.numeric(config$max_retries) && config$max_retries >= 0) {
        self$max_retries <- config$max_retries
      }
      
      if (is.null(self$api_key) || self$api_key == "") {
        stop(
          "\n\n╭────────────────────── Claude API Key Not Found ──────────────────────╮\n",
          "│                                                                       │\n",
          "│ You need to set up your Claude API key before using this provider.    │\n",
          "│                                                                       │\n",
          "│ To set up your API key, run:                                          │\n",
          "│ tldr_config(api_key = \"your_claude_api_key\")                          │\n",
          "│                                                                       │\n",
          "│ You can get an API key from: https://console.anthropic.com/           │\n",
          "│                                                                       │\n",
          "│ Alternatively, use the \"openai\" provider if you have that key:        │\n",
          "│ tldr(\"function_name\", provider = \"openai\")                           │\n",
          "│                                                                       │\n",
          "╰───────────────────────────────────────────────────────────────────────╯"
        )
      }
      
      if (is.null(self$model) || !self$model %in% self$provider_models) {
        warning("Invalid or missing model. Using default model.")
        self$model <- "claude-3-opus-20240229"
      }
      
      invisible(self)
    },
    
    #' @description Get response from Claude API
    #' @param prompt The prompt to send
    #' @return API response text
    get_response = function(prompt) {
      # Construct API request
      response <- httr2::request("https://api.anthropic.com/v1/messages") |>
        httr2::req_headers(
          "x-api-key" = self$api_key,
          "anthropic-version" = "2023-06-01",
          "content-type" = "application/json"
        ) |>
        httr2::req_body_json(list(
          model = self$model,
          max_tokens = 1024,
          temperature = 0.3,
          messages = list(
            list(
              role = "user",
              content = prompt
            )
          )
        )) |>
        httr2::req_retry(
          max_tries = self$max_retries,
          backoff = ~ 2 ^ .x,  # Exponential backoff (2, 4, 8 seconds...)
          is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
        ) |>
        httr2::req_timeout(seconds = self$timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
      
      # Parse response
      response_data <- httr2::resp_body_json(response)
      
      # Check for API errors
      if (httr2::resp_status(response) != 200) {
        error_msg <- paste("API error:", httr2::resp_status(response), 
                        response_data$error$type, response_data$error$message)
        stop(error_msg)
      }
      
      # Extract the content from the response
      response_data$content[[1]]$text
    },
    
    #' @description Check if authentication is valid
    #' @return Logical indicating if auth is valid
    check_auth = function() {
      tryCatch({
        # Simple API call to test authentication
        response <- httr2::request("https://api.anthropic.com/v1/models") |>
          httr2::req_headers("x-api-key" = self$api_key) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
        
        return(httr2::resp_status(response) == 200)
      }, error = function(e) {
        return(FALSE)
      })
    }
  )
)

#' OpenAI API Provider
#'
#' @description Provider implementation for OpenAI API
#'
#' @keywords internal
OpenAIProvider <- R6::R6Class("OpenAIProvider",
  inherit = LLMProvider,
  public = list(
    #' @field api_key API key
    api_key = NULL,
    #' @field model Model name
    model = NULL,
    #' @field max_retries Maximum number of retries
    max_retries = 3,
    
    #' @field timeout API request timeout in seconds
    timeout = 60,
    
    #' @description Initialize a new OpenAI provider
    #' @param config Configuration list
    initialize = function(config) {
      self$provider_name <- "openai"
      self$provider_models <- c(
        "gpt-4o",
        "gpt-4-turbo",
        "gpt-4",
        "gpt-3.5-turbo"
      )
      
      # Set API key, model, and performance settings
      self$api_key <- config$openai_api_key
      self$model <- config$openai_model
      
      # Set custom timeout and max_retries if provided
      if (!is.null(config$timeout) && is.numeric(config$timeout) && config$timeout > 0) {
        self$timeout <- config$timeout
      }
      
      if (!is.null(config$max_retries) && is.numeric(config$max_retries) && config$max_retries >= 0) {
        self$max_retries <- config$max_retries
      }
      
      if (is.null(self$api_key) || self$api_key == "") {
        stop(
          "\n\n╭────────────────────── OpenAI API Key Not Found ──────────────────────╮\n",
          "│                                                                       │\n",
          "│ You need to set up your OpenAI API key before using this provider.    │\n",
          "│                                                                       │\n",
          "│ To set up your API key, run:                                          │\n",
          "│ tldr_config(openai_api_key = \"your_openai_api_key\")                   │\n",
          "│                                                                       │\n",
          "│ You can get an API key from: https://platform.openai.com/api-keys     │\n",
          "│                                                                       │\n",
          "│ Alternatively, use the \"claude\" provider if you have that key:        │\n",
          "│ tldr(\"function_name\", provider = \"claude\")                           │\n",
          "│                                                                       │\n",
          "╰───────────────────────────────────────────────────────────────────────╯"
        )
      }
      
      if (is.null(self$model) || !self$model %in% self$provider_models) {
        warning("Invalid or missing model. Using default model.")
        self$model <- "gpt-4-turbo"
      }
      
      invisible(self)
    },
    
    #' @description Get response from OpenAI API
    #' @param prompt The prompt to send
    #' @return API response text
    get_response = function(prompt) {
      # Construct API request
      response <- httr2::request("https://api.openai.com/v1/chat/completions") |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", self$api_key),
          "Content-Type" = "application/json"
        ) |>
        httr2::req_body_json(list(
          model = self$model,
          messages = list(
            list(
              role = "user",
              content = prompt
            )
          ),
          max_tokens = 1024,
          temperature = 0.3
        )) |>
        httr2::req_retry(
          max_tries = self$max_retries,
          backoff = ~ 2 ^ .x,  # Exponential backoff (2, 4, 8 seconds...)
          is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
        ) |>
        httr2::req_timeout(seconds = self$timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
      
      # Parse response
      response_data <- httr2::resp_body_json(response)
      
      # Check for API errors
      if (httr2::resp_status(response) != 200) {
        error_msg <- paste("API error:", httr2::resp_status(response),
                          ifelse(!is.null(response_data$error$message), response_data$error$message, "Unknown error"))
        stop(error_msg)
      }
      
      # Extract the content from the response
      response_data$choices[[1]]$message$content
    },
    
    #' @description Check if authentication is valid
    #' @return Logical indicating if auth is valid
    check_auth = function() {
      tryCatch({
        # Simple API call to test authentication
        response <- httr2::request("https://api.openai.com/v1/models") |>
          httr2::req_headers("Authorization" = paste("Bearer", self$api_key)) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
        
        return(httr2::resp_status(response) == 200)
      }, error = function(e) {
        return(FALSE)
      })
    }
  )
)

#' LLM Provider Factory
#'
#' @description Factory class to create appropriate LLM providers
#'
#' @keywords internal
LLMProviderFactory <- R6::R6Class("LLMProviderFactory",
  public = list(
    #' @description Create a provider instance
    #' @param provider_name Name of the provider
    #' @param config Configuration
    #' @return Provider instance
    create_provider = function(provider_name, config) {
      if (provider_name == "claude") {
        return(ClaudeProvider$new(config))
      } else if (provider_name == "openai") {
        return(OpenAIProvider$new(config))
      } else {
        stop(
          "\n\n╭────────────────────── Invalid Provider Specified ────────────────────╮\n",
          "│                                                                       │\n",
          "│ The provider '", provider_name, "' is not supported.                        \n",
          "│                                                                       │\n",
          "│ Supported providers are:                                              │\n",
          "│ - \"claude\"  (default)                                                 │\n",
          "│ - \"openai\"                                                            │\n",
          "│                                                                       │\n",
          "│ Example usage:                                                        │\n",
          "│ tldr(\"mean\", provider = \"claude\")                                    │\n",
          "│ tldr(\"dplyr::filter\", provider = \"openai\")                           │\n",
          "│                                                                       │\n",
          "╰───────────────────────────────────────────────────────────────────────╯"
        )
      }
    }
  )
)

#' Get AI response from selected LLM provider
#'
#' @param prompt The prompt text to send to the API
#' @param provider_override Optional override for the provider to use
#' @param async Logical indicating whether to make the request asynchronously
#'
#' @return The API response text
#' @keywords internal
get_ai_response <- function(prompt, provider_override = NULL, async = FALSE) {
  config <- get_config_all()
  
  # Determine which provider to use
  provider_name <- provider_override %||% get_config("provider", default = "claude")
  
  # Create provider instance
  factory <- LLMProviderFactory$new()
  provider <- tryCatch({
    factory$create_provider(provider_name, config)
  }, error = function(e) {
    # If the requested provider fails, try fallback if enabled
    if (get_config("enable_fallback", default = FALSE) && 
        provider_name != get_config("fallback_provider", default = "claude")) {
      warning("Primary provider failed: ", e$message, 
              ". Trying fallback provider: ", get_config("fallback_provider"))
      factory$create_provider(get_config("fallback_provider"), config)
    } else {
      stop(e)
    }
  })
  
  # Check whether to show progress
  show_progress <- get_config("show_progress", default = TRUE)
  
  # Initialize progress bar if needed
  if (show_progress) {
    provider_display <- ifelse(provider_name == "claude", "Claude's API", "OpenAI's API")
    message(paste0("Querying ", provider_display, "..."))
  }
  
  # Check if cached response exists first before making API calls
  if (get_config("cache_enabled", default = TRUE)) {
    # Generate environment fingerprint for context-aware responses
    env_fingerprint <- NULL
    if (grepl("CONTEXT NOTE|Context awareness mode is set to: YES", prompt, fixed = FALSE)) {
      # For context-aware prompts, include environment data in cache key
      # This ensures the cache updates when environment (like data frames) changes
      env_objects <- ls(envir = .GlobalEnv)
      dfs <- env_objects[sapply(env_objects, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
      if (length(dfs) > 0) {
        df_info <- lapply(dfs, function(df_name) {
          df <- get(df_name, envir = .GlobalEnv)
          list(
            name = df_name,
            dims = dim(df),
            col_names = colnames(df)
          )
        })
        env_fingerprint <- df_info
      }
    }
    
    # Create cache key from prompt, provider, and environment fingerprint (if context-aware)
    cache_key <- digest::digest(list(
      prompt = prompt, 
      provider = provider_name,
      env_fingerprint = env_fingerprint
    ), algo = "sha256")
    
    cache_path <- file.path(get_config("cache_dir"), paste0(cache_key, ".rds"))
    
    # Use cached response if available and not in refresh mode
    if (file.exists(cache_path) && !get_config("refresh_mode", default = FALSE)) {
      # Check if cache is expired
      cache_ttl <- get_config("cache_ttl", default = 30)
      file_time <- file.info(cache_path)$mtime
      now <- Sys.time()
      
      if (difftime(now, file_time, units = "days") <= cache_ttl) {
        # Read and validate cached response
        cached_response <- readRDS(cache_path)
        
        # Ensure the cached response doesn't contain template placeholders
        if (!is.null(cached_response) && 
            !grepl("\\{\\{FUNCTION_NAME\\}\\}", cached_response, fixed = TRUE) && 
            !grepl("\\{\\{FUNCTION_DESCRIPTION\\}\\}", cached_response, fixed = TRUE) && 
            !grepl("\\{\\{FUNCTION_ARGS\\}\\}", cached_response, fixed = TRUE) &&
            !grepl("\\{\\{EXAMPLES\\}\\}", cached_response, fixed = TRUE) &&
            nchar(cached_response) > 50) {
          
          if (show_progress) {
            message("Using cached response ✓")
          }
          return(cached_response)
        } else {
          if (get_config("debug_mode", default = FALSE)) {
            message("DEBUG: Found invalid cached response, ignoring")
          }
          # Continue to make an API request
        }
      }
    }
  }
  
  # Function to handle the API request
  make_request <- function() {
    tryCatch({
      result <- provider$get_response(prompt)
      
      # Show completion message
      if (show_progress) {
        provider_display <- ifelse(provider_name == "claude", "Claude's API", "OpenAI's API")
        message(paste0("Response received from ", provider_display, " ✓"))
      }
      
      # Cache the successful response
      if (get_config("cache_enabled", default = TRUE)) {
        # Validate response - don't cache template placeholders or empty responses
        if (!is.null(result) && 
            !grepl("\\{\\{FUNCTION_NAME\\}\\}", result, fixed = TRUE) && 
            !grepl("\\{\\{FUNCTION_DESCRIPTION\\}\\}", result, fixed = TRUE) && 
            !grepl("\\{\\{FUNCTION_ARGS\\}\\}", result, fixed = TRUE) &&
            !grepl("\\{\\{EXAMPLES\\}\\}", result, fixed = TRUE) &&
            nchar(result) > 50) {
          
          # We use digest to create a unique hash based on the prompt, provider, and environment
          # This ensures different functions with similar names don't collide
          # and that context-aware responses update when the environment changes
          
          # Generate environment fingerprint for context-aware responses
          env_fingerprint <- NULL
          if (grepl("CONTEXT NOTE|Context awareness mode is set to: YES", prompt, fixed = FALSE)) {
            # For context-aware prompts, include environment data in cache key
            env_objects <- ls(envir = .GlobalEnv)
            dfs <- env_objects[sapply(env_objects, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
            if (length(dfs) > 0) {
              df_info <- lapply(dfs, function(df_name) {
                df <- get(df_name, envir = .GlobalEnv)
                list(
                  name = df_name,
                  dims = dim(df),
                  col_names = colnames(df)
                )
              })
              env_fingerprint <- df_info
            }
          }
          
          cache_key <- digest::digest(list(
            prompt = prompt, 
            provider = provider$provider_name,
            env_fingerprint = env_fingerprint
          ), algo = "sha256")
          cache_path <- file.path(get_config("cache_dir"), paste0(cache_key, ".rds"))
          
          # Ensure cache directory exists
          if (!dir.exists(get_config("cache_dir"))) {
            dir.create(get_config("cache_dir"), recursive = TRUE)
          }
          
          saveRDS(result, cache_path)
        } else {
          if (get_config("debug_mode", default = FALSE)) {
            message("DEBUG: Not caching invalid or template response")
          }
        }
      }
      
      result
    }, error = function(e) {
      # Show error message
      if (show_progress) {
        provider_display <- ifelse(provider_name == "claude", "Claude's API", "OpenAI's API")
        message(paste0("Error querying ", provider_display, " ✗"))
      }
      
      if (get_config("enable_fallback", default = FALSE) && 
          provider_name != get_config("fallback_provider", default = "claude")) {
        warning("Primary provider request failed: ", e$message, 
                ". Trying fallback provider: ", get_config("fallback_provider"))
        
        # Show fallback message
        if (show_progress) {
          fallback_display <- ifelse(
            get_config("fallback_provider") == "claude", 
            "Claude's API (fallback)", 
            "OpenAI's API (fallback)"
          )
          message(paste0("Trying fallback: ", fallback_display, "..."))
        }
        
        fallback_provider <- factory$create_provider(get_config("fallback_provider"), config)
        fallback_result <- tryCatch({
          result <- fallback_provider$get_response(prompt)
          
          # Show fallback success message
          if (show_progress) {
            message(paste0("Response received from ", fallback_display, " ✓"))
          }
          
          result
        }, error = function(fallback_err) {
          # Show fallback error message
          if (show_progress) {
            message(paste0("Error querying ", fallback_display, " ✗"))
          }
          
          # Continue with the error handling below
          stop(fallback_err)
        })
        
        return(fallback_result)
      } else {
        # Use cached response if available
        cache_key <- digest::digest(list(prompt = prompt, provider = provider_name), algo = "sha256")
        cache_path <- file.path(get_config("cache_dir"), paste0(cache_key, ".rds"))
        
        if (file.exists(cache_path) && get_config("offline_mode", default = FALSE)) {
          message("Using cached response in offline mode")
          return(readRDS(cache_path))
        } else {
          stop(e)
        }
      }
    })
  }
  
  # Execute the request either synchronously or asynchronously
  if (async && requireNamespace("future", quietly = TRUE) && 
      requireNamespace("promises", quietly = TRUE)) {
    future_result <- future::future(make_request())
    return(future_result)
  } else {
    return(make_request())
  }
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}