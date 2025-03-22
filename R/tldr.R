#' Get AI-powered quick reference for R functions
#'
#' @param func_name Character string specifying the name of an R function
#' @param verbose Logical indicating whether to include more detailed information
#' @param examples Integer indicating the number of examples to include
#' @param refresh Logical indicating whether to ignore cached results
#' @param provider Character string specifying the LLM provider to use ("claude" or "openai")
#' @param voice Character string specifying the character voice to use (e.g., "enthusiastic_explorer")
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
#' }
tldr <- function(func_name, verbose = NULL, examples = NULL, refresh = FALSE, 
                provider = NULL, voice = NULL) {
  # Validate input
  if (!is.character(func_name) || length(func_name) != 1) {
    stop("func_name must be a single character string")
  }
  
  # Use defaults if NULL
  if (is.null(verbose)) verbose <- get_config("verbose_default", default = FALSE)
  if (is.null(examples)) examples <- get_config("examples_default", default = 2)
  if (is.null(voice)) voice <- get_config("character_voice", default = "none")
  
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
    get_function_metadata(func_name, pkg)
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
  
  # Get API response
  response <- get_ai_response(prompt, provider_override = selected_provider)
  
  # Store provider info for output formatting
  attr(response, "provider") <- selected_provider
  
  # Apply character voice transformation if selected
  if (voice != "none") {
    response <- apply_character_voice(response, voice)
    attr(response, "voice") <- voice
  }
  
  # Print formatted response
  print_tldr_response(response, func_name, verbose, examples, selected_provider, voice)
  
  invisible(response)
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}