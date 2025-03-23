#' Analyze and explain R errors using AI
#'
#' This function analyzes R error messages or error objects, provides clear explanations 
#' of what went wrong, and suggests practical solutions. It can work with error messages as strings,
#' error objects from tryCatch, or automatically capture the last error from your R session.
#'
#' @param error Error message (character string) or error object to explain. If NULL and
#'        auto_capture=TRUE, the function will attempt to get the last error from the session.
#' @param code Optional character string containing the code that produced the error. 
#'        This helps provide more accurate explanations and fixes.
#' @param context Logical indicating whether to use context awareness. When TRUE,
#'        examines your R environment to provide more relevant explanations.
#'        Default is TRUE.
#' @param recommend Logical indicating whether to provide fix recommendations.
#'        Default is TRUE.
#' @param examples Integer specifying the number of correction examples to provide.
#'        Default is 2.
#' @param voice Character string specifying the character voice to use. Use
#'        \code{\link{tldr_list_voices}} to see available options. Default is NULL (uses the 
#'        configured default).
#' @param provider Character string specifying the LLM provider to use. Options are "claude" (default)
#'        or "openai". Requires API key to be configured for the selected provider.
#' @param refresh Logical indicating whether to ignore cached results and generate a fresh response.
#'        Default is FALSE.
#' @param auto_capture Logical indicating whether to automatically capture the last error if none provided.
#'        Default is FALSE.
#'
#' @return Prints formatted explanation to the console and invisibly returns the raw response
#'         as a character string. The returned object has attributes that can be accessed with attr():
#'         \itemize{
#'           \item "provider": The LLM provider used ("claude" or "openai")
#'           \item "voice": The character voice used (if any)
#'           \item "context_aware": TRUE if contextual awareness was used
#'         }
#'
#' @seealso
#' \code{\link{tldr}} for explaining R functions
#' \code{\link{tldr_explain}} for explaining R code
#' \code{\link{tldr_config}} for configuration options
#' \code{\link{tldr_list_voices}} for available character voices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Explain a simple error message directly
#' tldr_error("Error in mean(x) : object 'x' not found")
#' 
#' # Analyze an error after it occurs
#' tryCatch(mean(nonexistent_variable), 
#'          error = function(e) tldr_error(e))
#' 
#' # Automatically capture the last error that occurred
#' mean(nonexistent_variable)  # This will error
#' tldr_error(auto_capture = TRUE)
#' 
#' # Provide code for better context
#' tldr_error("Error: could not find function '%>%'", 
#'            code = "data %>% filter(x > 5)")
#' 
#' # Different providers and voices
#' tldr_error("Error in library(nonexistent) : there is no package called 'nonexistent'", 
#'            voice = "professor")
#' 
#' # Disable recommendations
#' tldr_error("Error in read.csv(file) : object 'file' not found", 
#'            recommend = FALSE)
#' 
#' # Request more examples
#' tldr_error("Error in ggplot2::ggplot() : could not find function 'ggplot2::ggplot'", 
#'            examples = 3)
#' }
tldr_error <- function(error = NULL, code = NULL, context = TRUE, recommend = TRUE,
                      examples = 2, voice = NULL, provider = NULL, refresh = FALSE,
                      auto_capture = FALSE) {
  
  # Handle auto-capture of last error if requested
  if (is.null(error) && auto_capture) {
    error <- capture_last_error()
    if (is.null(error)) {
      stop("No recent error found to analyze. Run this function immediately after an error occurs or provide an error message/object.")
    }
  }
  
  # Validate input
  if (is.null(error)) {
    stop("Please provide an error message or error object to explain.")
  }
  
  # Process error input (string or error object)
  error_info <- process_error_input(error)
  
  # Use defaults if NULL
  if (is.null(voice)) voice <- get_config("character_voice", default = "none")
  
  # Set refresh mode temporarily in the config to be used by get_ai_response
  if (refresh) {
    old_refresh_mode <- get_config("refresh_mode", default = FALSE)
    tldr_config(refresh_mode = TRUE)
    on.exit(tldr_config(refresh_mode = old_refresh_mode))
  }
  
  # Determine which provider to use
  selected_provider <- provider %||% get_config("provider", default = "claude")
  
  # Generate a unique cache key based on the error, code, and options
  cache_key <- digest::digest(
    list(
      error_message = error_info$message,
      error_call = error_info$call,
      error_trace = error_info$traceback,
      code = code,
      recommend = recommend,
      examples = examples,
      context = context,
      provider = selected_provider
    ),
    algo = "sha256"
  )
  
  # Create cache path
  cache_dir <- get_config("cache_dir")
  cache_path <- file.path(cache_dir, paste0("error_", cache_key, ".rds"))
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Error message: ", error_info$message)
    if (!is.null(error_info$call)) {
      message("DEBUG: Error call: ", deparse(error_info$call))
    }
    if (!is.null(error_info$traceback)) {
      message("DEBUG: Error traceback length: ", length(error_info$traceback))
    }
    message("DEBUG: Code provided: ", !is.null(code))
    message("DEBUG: Context enabled: ", context)
    message("DEBUG: Recommendations: ", recommend)
    message("DEBUG: Examples: ", examples)
    message("DEBUG: Provider: ", selected_provider)
    message("DEBUG: Cache path: ", cache_path)
  }
  
  # Check for cached response (if not using context awareness)
  if (!refresh && !context && file.exists(cache_path)) {
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
      
      print_error_response(response, error_info, code, recommend, examples, 
                          selected_provider, voice)
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
      
      print_error_response(response, error_info, code, recommend, examples,
                         selected_provider, voice)
      return(invisible(response))
    }
    # Otherwise continue to get a fresh response
  } else if ((refresh || context) && get_config("offline_mode", default = FALSE)) {
    if (context) {
      stop("Cannot use context awareness in offline mode. Disable offline mode first with tldr_offline(FALSE).")
    } else {
      stop("Cannot refresh in offline mode. Disable offline mode first with tldr_offline(FALSE).")
    }
  }
  
  # Handle contextual awareness
  context_data <- NULL
  if (context) {
    # Get context settings
    config <- get_config_all()
    context_settings <- config$context_settings %||% list(
      enable_context_awareness = TRUE,
      analyze_data_frames = TRUE,
      analyze_packages = TRUE,
      analyze_history = TRUE,
      anonymize_data = TRUE,
      max_rows_sample = 5,
      max_cols_sample = 5,
      include_row_count = TRUE,
      include_class_info = TRUE,
      include_column_types = TRUE,
      max_history_commands = 10
    )
    
    # Create context analyzer and analyze environment
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Analyzing user environment context")
    }
    
    # Initialize context analyzer with current settings
    context_analyzer <- ContextAnalyzer$new(context_settings)
    
    # Analyze the environment
    context_analyzer$analyze_environment()
    
    # Extract relevant context from code if provided
    if (!is.null(code)) {
      code_vars <- extract_variables_from_code(code)
      code_funcs <- extract_functions_from_code(code)
    } else if (!is.null(error_info$call)) {
      # Try to extract from the error call
      call_str <- deparse(error_info$call)
      code_vars <- extract_variables_from_code(call_str)
      code_funcs <- extract_functions_from_code(call_str)
    } else {
      code_vars <- character(0)
      code_funcs <- character(0)
    }
    
    # Create dummy metadata for context awareness
    dummy_func_name <- "error_analysis"
    
    # Extract potential package names from error or functions
    potential_pkg <- NULL
    
    # Try to identify packages from the error message
    if (grepl("package", error_info$message, ignore.case = TRUE)) {
      pkg_match <- regmatches(
        error_info$message,
        regexpr("package ['\"]?([A-Za-z0-9.]+)['\"]?", error_info$message)
      )
      if (length(pkg_match) > 0) {
        pkg_name <- gsub("package ['\"]?([A-Za-z0-9.]+)['\"]?", "\\1", pkg_match)
        potential_pkg <- pkg_name
      }
    } else if (length(code_funcs) > 0) {
      # Try to identify packages from functions
      # Check some common package patterns
      dplyr_funcs <- c("filter", "select", "mutate", "summarize", "group_by", "arrange")
      tidyr_funcs <- c("pivot_longer", "pivot_wider", "separate", "unite", "nest", "unnest")
      ggplot_funcs <- c("ggplot", "geom_", "aes", "facet_", "theme", "scale_")
      
      # Check for presence of functions from common packages
      if (any(sapply(dplyr_funcs, function(f) any(grepl(f, code_funcs))))) {
        potential_pkg <- "dplyr"
      } else if (any(sapply(tidyr_funcs, function(f) any(grepl(f, code_funcs))))) {
        potential_pkg <- "tidyr"
      } else if (any(sapply(ggplot_funcs, function(f) any(grepl(f, code_funcs))))) {
        potential_pkg <- "ggplot2"
      } else {
        potential_pkg <- "base"  # Default to base R
      }
    } else {
      potential_pkg <- "base"  # Default to base R
    }
    
    dummy_func_metadata <- list(
      name = dummy_func_name,
      package = potential_pkg,
      signature = "",
      args = list(),
      keywords = code_funcs,
      related_vars = code_vars
    )
    
    # Format context data for prompt
    context_data <- context_analyzer$format_context_for_prompt(dummy_func_name, dummy_func_metadata)
    
    if (length(code_vars) > 0 || length(code_funcs) > 0) {
      context_data <- paste0(
        context_data,
        "\n\nCODE SPECIFIC CONTEXT:\n",
        if (length(code_vars) > 0) paste0("Variables in code: ", paste(code_vars, collapse = ", "), "\n") else "",
        if (length(code_funcs) > 0) paste0("Functions in code: ", paste(code_funcs, collapse = ", "), "\n") else ""
      )
    }
    
    # Add packages information
    loaded_pkgs <- loadedNamespaces()
    if (length(loaded_pkgs) > 0) {
      context_data <- paste0(
        context_data,
        "\n\nLOADED PACKAGES:\n",
        paste(loaded_pkgs, collapse = ", ")
      )
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Context data generated successfully")
    }
  }
  
  # Build the prompt
  prompt <- build_error_prompt(error_info, code, recommend, examples, context_data)
  
  # Get API response in offline mode?
  if (get_config("offline_mode", default = FALSE) && !file.exists(cache_path)) {
    stop("Error explanation not cached and offline mode is enabled. Disable offline mode to fetch response.")
  }
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generated prompt (truncated):")
    message(substr(prompt, 1, 300), "...")
  }
  
  # Get API response
  response_obj <- get_ai_response(prompt, provider_override = selected_provider)
  
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
  
  # Add context-aware flag if used
  if (context) {
    attr(response_obj, "context_aware") <- TRUE
  }
  
  # Cache the response
  if (get_config("cache_enabled", default = TRUE) && !context) {
    if (!is.null(response_obj) && !grepl("\\{\\{ERROR_MESSAGE\\}\\}", response_obj, fixed = TRUE) &&
        nchar(response_obj) > 50) {
      # Ensure cache directory exists
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      
      saveRDS(response_obj, cache_path)
    }
  }
  
  # Print formatted response
  print_error_response(response_obj, error_info, code, recommend, examples,
                      selected_provider, voice)
  
  invisible(response_obj)
}

#' Build the prompt for error explanation
#'
#' @param error_info Processed error information (list with message, call, traceback)
#' @param code The code that produced the error (if available)
#' @param recommend Whether to provide fix recommendations
#' @param examples Number of examples to request
#' @param context_data Optional context data from the context analyzer
#'
#' @return Character string containing the prompt
#' @keywords internal
build_error_prompt <- function(error_info, code, recommend, examples, context_data = NULL) {
  # Get the explanation template
  template <- get_error_template()
  
  # Replace placeholders with actual values
  template <- gsub("{{ERROR_MESSAGE}}", error_info$message, template, fixed = TRUE)
  
  # Handle error call if available
  if (!is.null(error_info$call)) {
    call_str <- deparse(error_info$call)
    template <- gsub("{{ERROR_CALL}}", call_str, template, fixed = TRUE)
    template <- gsub("{{HAS_ERROR_CALL}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{ERROR_CALL}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_ERROR_CALL}}", "NO", template, fixed = TRUE)
  }
  
  # Handle traceback if available
  if (!is.null(error_info$traceback) && length(error_info$traceback) > 0) {
    trace_str <- paste(error_info$traceback, collapse = "\n")
    template <- gsub("{{ERROR_TRACEBACK}}", trace_str, template, fixed = TRUE)
    template <- gsub("{{HAS_ERROR_TRACEBACK}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{ERROR_TRACEBACK}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_ERROR_TRACEBACK}}", "NO", template, fixed = TRUE)
  }
  
  # Handle code if available
  if (!is.null(code)) {
    template <- gsub("{{CODE}}", code, template, fixed = TRUE)
    template <- gsub("{{HAS_CODE}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{CODE}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_CODE}}", "NO", template, fixed = TRUE)
  }
  
  # Handle recommendations setting
  recommend_str <- ifelse(recommend, "YES", "NO")
  template <- gsub("{{RECOMMEND}}", recommend_str, template, fixed = TRUE)
  
  # Handle examples count
  template <- gsub("{{EXAMPLES_REQUESTED}}", as.character(examples), template, fixed = TRUE)
  
  # Add context data if available
  if (!is.null(context_data)) {
    template <- gsub("{{CONTEXT_DATA}}", context_data, template, fixed = TRUE)
    template <- gsub("{{HAS_CONTEXT}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{CONTEXT_DATA}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_CONTEXT}}", "NO", template, fixed = TRUE)
  }
  
  template
}

#' Get the template for error explanation
#'
#' @return Character string containing the error explanation template
#' @keywords internal
get_error_template <- function() {
  return('
You are tldrAI, a tool that provides clear, practical explanations of R errors.
Your goal is to analyze the provided R error, explain what went wrong, and suggest practical solutions.

Here is the R error to explain:
```
{{ERROR_MESSAGE}}
```

ERROR CALL INFORMATION: {{HAS_ERROR_CALL}}
```
{{ERROR_CALL}}
```

ERROR TRACEBACK: {{HAS_ERROR_TRACEBACK}}
```
{{ERROR_TRACEBACK}}
```

CODE THAT PRODUCED THE ERROR: {{HAS_CODE}}
```r
{{CODE}}
```

Recommendations requested: {{RECOMMEND}}
Examples requested: {{EXAMPLES_REQUESTED}}

Context awareness mode is set to: {{HAS_CONTEXT}}
{{CONTEXT_DATA}}

Please provide a response in the following format:

```
# Error Analysis

## What Went Wrong
A clear explanation of what the error means and what caused it. Explain in user-friendly terms, not just restating the error message.

## Error Type
Identify the general category of this error (e.g., syntax error, missing package, object not found, type mismatch, etc.)

{{#if RECOMMEND is YES}}
## How To Fix It
Provide practical steps to fix the error, addressing the root cause.

## {{EXAMPLES_REQUESTED}} Correction Example(s)
```r
# Example 1: [Brief description]
[corrected code example]
```

```r
# Example 2: [Brief description if more than one example]
[corrected code example if more than one example]
```
{{/if}}

## Related Tips
Additional advice to avoid similar errors in the future or improve the code.
```

Please ensure your response is:
1. Clear and practical - focus on solving the problem, not just explaining the theory
2. Educational - help the user understand why the error occurred, not just how to fix it
3. Accurate - only suggest solutions that will actually work
4. Contextual - if context awareness mode is YES, incorporate information from the user\'s environment

For common error patterns:
- "object not found" - Check for typos, missing variables, unloaded packages
- "could not find function" - Check for typos, missing package loads, namespace issues
- "package not found" - Suggest installation commands
- "unexpected token" - Identify syntax issues like missing parentheses, brackets
- "non-numeric argument to binary operator" - Explain type mismatches
- "subscript out of bounds" - Explain indexing issues
- "no applicable method" - Explain S3/S4 method dispatch issues
- "unused argument" - Identify incorrect function parameters

If context awareness mode is YES:
1. Check which packages the user has loaded that might be relevant
2. Reference their actual data frames and variables if they relate to the error
3. Consider their available objects when suggesting solutions
4. Tailor examples to work with their actual environment

{{#if HAS_CODE is YES}}
When the code that produced the error is provided, analyze it line by line to pinpoint the issue.
{{/if}}

{{#if HAS_ERROR_TRACEBACK is YES}}
When a traceback is provided, use it to understand the sequence of function calls that led to the error.
{{/if}}

If recommending package installation, use install.packages() with quotes: install.packages("package_name")
')
}

#' Process error input into a standardized format
#'
#' @param error Error input (string or error object)
#'
#' @return List with standardized error information
#' @keywords internal
process_error_input <- function(error) {
  # Initialize result
  result <- list(
    message = NULL,
    call = NULL,
    traceback = NULL
  )
  
  # Process based on input type
  if (is.character(error)) {
    # Handle string error message
    result$message <- error
  } else if (inherits(error, "error") || inherits(error, "condition")) {
    # Handle error object
    result$message <- conditionMessage(error)
    result$call <- conditionCall(error)
    
    # Try to get traceback if available
    if (!is.null(attr(error, "traceback"))) {
      result$traceback <- attr(error, "traceback")
    }
  } else if (is.list(error) && !is.null(error$message)) {
    # Handle error-like list (e.g., from try())
    result$message <- error$message
    if (!is.null(error$call)) {
      result$call <- error$call
    }
  } else {
    # Convert other objects to string representation
    result$message <- paste0("Error: ", as.character(error)[1])
  }
  
  result
}

#' Capture the last error from the R session
#'
#' @return Error object or NULL if no recent error
#' @keywords internal
capture_last_error <- function() {
  # Get the last error message
  err_msg <- geterrmessage()
  
  # If empty, no recent error
  if (err_msg == "") {
    return(NULL)
  }
  
  # Create a simple error object
  err_obj <- list(
    message = err_msg,
    call = NULL
  )
  
  # Try to get the traceback
  tb <- tryCatch({
    # Get the last traceback if available
    sys.calls()
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(tb) && length(tb) > 0) {
    err_obj$traceback <- sapply(tb, deparse)
  }
  
  # Return the constructed error object
  err_obj
}

#' Print formatted error response
#'
#' @param response The raw response from the AI
#' @param error_info The processed error information
#' @param code The original code (if provided)
#' @param recommend Whether recommendations were requested
#' @param examples Number of examples requested
#' @param provider The LLM provider that generated the response
#' @param voice The character voice used (if any)
#'
#' @return Invisibly returns NULL
#' @keywords internal
print_error_response <- function(response, error_info, code, recommend, examples,
                               provider = NULL, voice = NULL) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Printing error response")
    message("DEBUG: Raw response length: ", nchar(response))
  }
  
  # Extract the code block content
  content <- extract_code_block(response)
  
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Extracted content length: ", nchar(content))
  }
  
  # Traditional console output format
  cat("\n")
  cli::cli_h1("tldrAI: Error Analysis")
  
  # Format the content
  formatted_content <- format_content(content)
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Formatted content length: ", nchar(formatted_content))
  }
  cat(formatted_content)
  
  # Add a footer
  cat("\n")
  provider_name <- provider %||% attr(response, "provider") %||% "claude"
  provider_display <- ifelse(provider_name == "claude", "Claude's API", "OpenAI's API")
  
  cli::cli_text("{.emph Generated by tldrAI using ", provider_display, "}")
  
  # Add character voice info if used
  voice_name <- voice %||% attr(response, "voice") %||% "none"
  if (voice_name != "none") {
    # Format voice name for display (replace underscores with spaces, capitalize words)
    display_voice <- gsub("_", " ", voice_name)
    display_voice <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", display_voice, perl = TRUE)
    
    cli::cli_text("{.emph Character voice: ", display_voice, "}")
  }
  
  # Add context awareness info if used
  context_aware <- attr(response, "context_aware") %||% FALSE
  if (context_aware) {
    cli::cli_text("{.emph Context awareness: Enabled}")
  }
  
  cat("\n")
  
  invisible(NULL)
}

#' Enable automatic error explanations
#'
#' @param enable Logical indicating whether to enable (TRUE) or disable (FALSE) automatic error explanations
#' @param verbose Logical indicating whether to show explanations automatically (TRUE) or only when requested (FALSE)
#'
#' @return Invisibly returns the previous error handler
#' @export
#'
#' @examples
#' \dontrun{
#' # Enable automatic error explanations
#' tldr_auto_error(TRUE)
#' 
#' # Try a function that will error
#' mean(nonexistent_variable)  # This will automatically show an explanation
#' 
#' # Disable automatic error explanations
#' tldr_auto_error(FALSE)
#' }
tldr_auto_error <- function(enable = TRUE, verbose = TRUE) {
  # Store the original error handler
  original_handler <- getOption("error")
  
  if (enable) {
    # Create a custom error handler
    custom_handler <- function() {
      # Capture the error
      err <- geterrmessage()
      
      # Get the call if possible
      calls <- sys.calls()
      call_obj <- if (length(calls) > 0) calls[[1]] else NULL
      
      # Process the error
      err_obj <- list(
        message = err,
        call = call_obj
      )
      
      # Add traceback if available
      tb <- tryCatch(sys.calls(), error = function(e) NULL)
      if (!is.null(tb) && length(tb) > 0) {
        err_obj$traceback <- sapply(tb, deparse)
      }
      
      if (verbose) {
        # Show the original error message first
        cat(err, "\n")
        
        # Then provide the explanation
        message("\nAutomatic error explanation (tldrAI):")
        tryCatch({
          tldr_error(err_obj, context = TRUE, refresh = FALSE)
        }, error = function(e) {
          message("Could not generate error explanation: ", e$message)
        })
      } else {
        message("Error occurred. Use tldr_error() for an explanation.")
      }
      
      # Invoke debugger if option is set
      if (identical(getOption("error"), quote(browser())))
        browser()
      
      # Return invisible NULL to continue
      invisible(NULL)
    }
    
    # Set the custom error handler
    options(error = custom_handler)
    message("Automatic error explanations enabled. Errors will now include AI-powered explanations.")
  } else {
    # Restore the default error handler
    options(error = original_handler)
    message("Automatic error explanations disabled. Use tldr_error() to explain errors manually.")
  }
  
  # Return the previous handler for potential restoration
  invisible(original_handler)
}

#' Extract code block from formatted text
#'
#' @param text Text containing code blocks
#'
#' @return Extracted content between code block markers
#' @keywords internal
extract_code_block <- function(text) {
  # First, try triple backtick blocks
  backtick_pattern <- "```(.*?)```"
  backtick_matches <- regmatches(text, gregexpr(backtick_pattern, text, dotall = TRUE))[[1]]
  
  if (length(backtick_matches) > 0) {
    # Extract the content of the first block (excluding the markers)
    content <- gsub("```(.*?)```", "\\1", backtick_matches[1], perl = TRUE)
    return(trimws(content))
  }
  
  # Fall back to the full text if no code blocks found
  return(text)
}

#' Format content with markdown-aware rendering
#'
#' @param content The content to format
#'
#' @return Formatted content string
#' @keywords internal
format_content <- function(content) {
  # Process markdown headings
  content <- gsub("^# (.+)$", function(m) cli::cli_h1(sub("^# ", "", m)), content)
  content <- gsub("^## (.+)$", function(m) cli::cli_h2(sub("^## ", "", m)), content)
  content <- gsub("^### (.+)$", function(m) cli::cli_h3(sub("^### ", "", m)), content)
  
  # Process code blocks (simple approach)
  content <- gsub("```r\\s*(.+?)\\s*```", function(m) {
    code <- sub("```r\\s*", "", m)
    code <- sub("\\s*```$", "", code)
    paste0("\n", cli::col_silver(code), "\n")
  }, content, perl = TRUE)
  
  # Process bullet points
  content <- gsub("^- (.+)$", function(m) paste0("• ", sub("^- ", "", m)), content)
  
  # Return the formatted content
  content
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}