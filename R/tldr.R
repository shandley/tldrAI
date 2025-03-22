#' Get AI-powered quick reference for R functions
#'
#' @param func_name Character string specifying the name of an R function
#' @param verbose Logical indicating whether to include more detailed information
#' @param examples Integer indicating the number of examples to include
#' @param refresh Logical indicating whether to ignore cached results
#' @param provider Character string specifying the LLM provider to use ("claude" or "openai")
#' @param voice Character string specifying the character voice to use (e.g., "enthusiastic_explorer")
#' @param async Logical indicating whether to make the API call asynchronously
#'
#' @return Prints formatted help to the console (invisibly returns the raw response)
#' @export
#'
#' @examples
#' \dontrun{
#' tldr("mean")
#' tldr("ggplot", examples = 3)
#' tldr("dplyr::filter", provider = "openai")
#' tldr("median", voice = "enthusiastic_explorer")
#' tldr("sd", voice = "cynical_detective")
#' tldr("plot", async = TRUE)  # Make an asynchronous API call
#' }
tldr <- function(func_name, verbose = NULL, examples = NULL, refresh = FALSE, 
                provider = NULL, voice = NULL, async = NULL) {
  # Validate input
  if (!is.character(func_name) || length(func_name) != 1) {
    stop("func_name must be a single character string")
  }
  
  # Use defaults if NULL
  if (is.null(verbose)) verbose <- get_config("verbose_default", default = FALSE)
  if (is.null(examples)) examples <- get_config("examples_default", default = 2)
  if (is.null(voice)) voice <- get_config("character_voice", default = "none")
  if (is.null(async)) async <- get_config("async_mode", default = FALSE)
  
  # Set refresh mode temporarily in the config to be used by get_ai_response
  if (refresh) {
    old_refresh_mode <- get_config("refresh_mode", default = FALSE)
    tldr_config(refresh_mode = TRUE)
    on.exit(tldr_config(refresh_mode = old_refresh_mode))
  }
  
  # Handle package::function format
  pkg <- NULL
  if (grepl("::", func_name, fixed = TRUE)) {
    parts <- strsplit(func_name, "::", fixed = TRUE)[[1]]
    pkg <- parts[1]
    func_name <- parts[2]
  }
  
  # Determine which provider to use
  selected_provider <- provider %||% get_config("provider", default = "claude")
  
  # Check for cached response
  cache_path <- get_cache_path(func_name, selected_provider)
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Function name: ", func_name)
    message("DEBUG: Provider: ", selected_provider)
    message("DEBUG: Cache path: ", cache_path)
    message("DEBUG: Async mode: ", async)
  }
  
  # Check for cached response first (moved to get_ai_response for centralized handling)
  if (!refresh && file.exists(cache_path)) {
    # Check if cache is expired
    cache_ttl <- get_config("cache_ttl", default = 30)
    file_time <- file.info(cache_path)$mtime
    now <- Sys.time()
    
    if (difftime(now, file_time, units = "days") <= cache_ttl) {
      response <- readRDS(cache_path)
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        response <- apply_character_voice(response, voice)
        attr(response, "voice") <- voice
      }
      
      print_tldr_response(response, func_name, verbose, examples, selected_provider, voice)
      return(invisible(response))
    } else if (get_config("offline_mode", default = FALSE)) {
      # In offline mode, use expired cache anyway
      response <- readRDS(cache_path)
      message("Using expired cached response (offline mode)")
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        response <- apply_character_voice(response, voice)
        attr(response, "voice") <- voice
      }
      
      print_tldr_response(response, func_name, verbose, examples, selected_provider, voice)
      return(invisible(response))
    }
    # Otherwise continue to get a fresh response
  } else if (refresh && get_config("offline_mode", default = FALSE)) {
    stop("Cannot refresh in offline mode. Disable offline mode first with tldr_offline(FALSE).")
  }
  
  # Get function metadata
  func_metadata <- tryCatch({
    metadata <- get_function_metadata(func_name, pkg)
    
    # Debug information if debug mode is enabled
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Function metadata:")
      message("DEBUG:   - Package: ", metadata$package)
      message("DEBUG:   - Signature: ", metadata$signature)
      message("DEBUG:   - Description: ", substr(metadata$description, 1, 50), "...")
    }
    
    metadata
  }, error = function(e) {
    if (get_config("offline_mode", default = FALSE) && file.exists(cache_path)) {
      message("Function not found, using cached response (offline mode)")
      response <- readRDS(cache_path)
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        response <- apply_character_voice(response, voice)
        attr(response, "voice") <- voice
      }
      
      print_tldr_response(response, func_name, verbose, examples, selected_provider, voice)
      return(invisible(response))
    } else {
      stop(e)
    }
  })
  
  # Build prompt
  prompt <- build_prompt(func_name, func_metadata, verbose, examples)
  
  # Get API response in offline mode?
  if (get_config("offline_mode", default = FALSE) && !file.exists(cache_path)) {
    stop("Function information not cached and offline mode is enabled. Disable offline mode to fetch response.")
  }
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    # Create a clean prompt with function name for debug output
    debug_prompt <- gsub("\\{\\{FUNCTION_NAME\\}\\}", func_name, prompt, fixed = TRUE)
    debug_prompt <- gsub("\\{\\{FUNCTION_SIGNATURE\\}\\}", func_metadata$signature, debug_prompt, fixed = TRUE)
    debug_prompt <- gsub("\\{\\{PACKAGE_NAME\\}\\}", func_metadata$package, debug_prompt, fixed = TRUE)
    
    message("DEBUG: Generated prompt:")
    message(debug_prompt)
  }
  
  # Handle async API calls
  if (async) {
    # Check if required packages are available
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("promises", quietly = TRUE)) {
      warning("Async mode requires the 'future' and 'promises' packages. Install with: install.packages(c(\"future\", \"promises\"))")
      async <- FALSE
    } else {
      # Initialize future if needed
      if (!getOption("future.initialized", FALSE)) {
        future::plan(future::multisession)
        options(future.initialized = TRUE)
      }
    }
  }
  
  # Get API response (with async support)
  response_obj <- get_ai_response(prompt, provider_override = selected_provider, async = async)
  
  # Handle async response
  if (async && inherits(response_obj, "FutureClass")) {
    message("Async request initiated. Use tldr_check_async() to retrieve the result.")
    
    # Register the response future for later retrieval
    assign("tldr_last_async_request", list(
      future = response_obj,
      func_name = func_name,
      verbose = verbose,
      examples = examples,
      provider = selected_provider,
      voice = voice,
      timestamp = Sys.time()
    ), envir = .GlobalEnv)
    
    return(invisible(response_obj))
  }
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: API Response (first 100 chars): ", substr(response_obj, 1, 100))
  }
  
  # Store provider info for output formatting
  attr(response_obj, "provider") <- selected_provider
  
  # Apply character voice transformation if selected
  if (voice != "none") {
    response_obj <- apply_character_voice(response_obj, voice)
    attr(response_obj, "voice") <- voice
  }
  
  # Print formatted response
  print_tldr_response(response_obj, func_name, verbose, examples, selected_provider, voice)
  
  invisible(response_obj)
}

#' Check and retrieve results from asynchronous tldr calls
#'
#' @param wait Logical indicating whether to wait for the result if not ready
#' @param timeout Numeric value specifying how long to wait for the result (in seconds)
#'
#' @return The result of the asynchronous tldr call
#' @export
#'
#' @examples
#' \dontrun{
#' tldr("mean", async = TRUE)
#' # Do other work...
#' result <- tldr_check_async()
#' }
tldr_check_async <- function(wait = TRUE, timeout = 30) {
  # Check if there is a registered async request
  if (!exists("tldr_last_async_request", envir = .GlobalEnv)) {
    stop("No asynchronous request found. Make a call with tldr(..., async = TRUE) first.")
  }
  
  # Get the async request object
  async_req <- get("tldr_last_async_request", envir = .GlobalEnv)
  
  # Check if ready
  is_ready <- future::resolved(async_req$future)
  
  if (!is_ready && !wait) {
    message("Async request is still processing. Set wait=TRUE to wait for completion.")
    return(invisible(NULL))
  } else if (!is_ready && wait) {
    message("Waiting for async request to complete (timeout: ", timeout, " seconds)...")
    
    start_time <- Sys.time()
    while (!future::resolved(async_req$future)) {
      Sys.sleep(0.5)
      if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
        stop("Async request timed out after ", timeout, " seconds. Try again later with tldr_check_async().")
      }
    }
  }
  
  # Get the result
  tryCatch({
    response <- future::value(async_req$future)
    
    # Store provider info for output formatting
    attr(response, "provider") <- async_req$provider
    
    # Apply character voice transformation if selected
    if (async_req$voice != "none") {
      response <- apply_character_voice(response, async_req$voice)
      attr(response, "voice") <- async_req$voice
    }
    
    # Print formatted response
    print_tldr_response(response, async_req$func_name, 
                      async_req$verbose, async_req$examples, 
                      async_req$provider, async_req$voice)
    
    # Clean up the async request
    rm("tldr_last_async_request", envir = .GlobalEnv)
    
    invisible(response)
  }, error = function(e) {
    # Clean up the async request on error
    rm("tldr_last_async_request", envir = .GlobalEnv)
    stop("Error in async request: ", e$message)
  })
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}