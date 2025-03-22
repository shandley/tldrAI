#' Get AI-powered quick reference for R functions
#'
#' @param func_name Character string specifying the name of an R function
#' @param verbose Logical indicating whether to include more detailed information
#' @param examples Integer indicating the number of examples to include
#' @param refresh Logical indicating whether to ignore cached results
#'
#' @return Prints formatted help to the console (invisibly returns the raw response)
#' @export
#'
#' @examples
#' \dontrun{
#' tldr("mean")
#' tldr("ggplot", examples = 3)
#' }
tldr <- function(func_name, verbose = FALSE, examples = 2, refresh = FALSE) {
  # Validate input
  if (!is.character(func_name) || length(func_name) != 1) {
    stop("func_name must be a single character string")
  }
  
  # Check for cached response
  cache_path <- get_cache_path(func_name)
  if (!refresh && file.exists(cache_path)) {
    response <- readRDS(cache_path)
    print_tldr_response(response, func_name, verbose, examples)
    return(invisible(response))
  }
  
  # Get function metadata
  func_metadata <- get_function_metadata(func_name)
  
  # Build prompt
  prompt <- build_prompt(func_name, func_metadata, verbose, examples)
  
  # Get API response
  response <- get_ai_response(prompt)
  
  # Cache response
  if (get_config("cache_enabled", default = TRUE)) {
    saveRDS(response, cache_path)
  }
  
  # Print formatted response
  print_tldr_response(response, func_name, verbose, examples)
  
  invisible(response)
}
