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
      
      # Set API key and model
      self$api_key <- config$api_key
      self$model <- config$model
      
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
        httr2::req_retry(max_tries = self$max_retries) |>
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
      
      # Set API key and model
      self$api_key <- config$openai_api_key
      self$model <- config$openai_model
      
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
        httr2::req_retry(max_tries = self$max_retries) |>
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
#'
#' @return The API response text
#' @keywords internal
get_ai_response <- function(prompt, provider_override = NULL) {
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
  
  # Try to get response
  response <- tryCatch({
    result <- provider$get_response(prompt)
    
    # Show completion message
    if (show_progress) {
      message(paste0("Response received from ", provider_display, " ✓"))
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
  
  # Cache the successful response
  if (get_config("cache_enabled", default = TRUE)) {
    # We use digest to create a unique hash based on the prompt and provider
    # This ensures different functions with similar names don't collide
    cache_key <- digest::digest(list(prompt = prompt, provider = provider$provider_name), algo = "sha256")
    cache_path <- file.path(get_config("cache_dir"), paste0(cache_key, ".rds"))
    
    # Ensure cache directory exists
    if (!dir.exists(get_config("cache_dir"))) {
      dir.create(get_config("cache_dir"), recursive = TRUE)
    }
    
    saveRDS(response, cache_path)
  }
  
  response
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}