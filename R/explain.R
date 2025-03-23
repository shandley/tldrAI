#' Explain R code using AI
#'
#' This function analyzes and explains R code, providing a detailed explanation of what
#' the code does, how it works, and why it's structured the way it is. It's useful for
#' understanding complex code, learning R, or documenting your own code.
#'
#' @param code Character string containing R code to explain, or a file path
#'        to an R script.
#' @param detail_level Character string specifying the level of detail in the explanation.
#'        Options are:
#'        \itemize{
#'          \item "basic" - Simple, high-level explanation (default)
#'          \item "intermediate" - More detailed explanation with some technical details
#'          \item "advanced" - In-depth explanation with detailed technical information
#'        }
#' @param highlight Logical indicating whether to include syntax highlighting in the output.
#'        Default is TRUE.
#' @param line_by_line Logical indicating whether to provide a line-by-line explanation.
#'        Default is FALSE. When TRUE, explains each significant line or block individually.
#' @param specific_lines Character string specifying a range of lines to explain (e.g., "5-10").
#'        Default is NULL, which explains the entire code.
#' @param context_aware Logical indicating whether to use context awareness. When TRUE,
#'        examines your R environment to provide a more relevant explanation. Default is FALSE.
#' @param voice Character string specifying the character voice to use. Use
#'        tldr_list_voices() to see available options. Default is NULL (uses the configured default).
#' @param refresh Logical indicating whether to ignore cached results and generate a fresh response.
#'        Default is FALSE.
#' @param provider Character string specifying the LLM provider to use. Options are "claude" (default)
#'        or "openai". Requires API key to be configured for the selected provider.
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
#' \code{\link{tldr_config}} for configuration options
#' \code{\link{tldr_list_voices}} for available character voices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Explain code from a string
#' code <- 'mtcars %>%
#'   filter(mpg > 20) %>%
#'   group_by(cyl) %>%
#'   summarize(avg_mpg = mean(mpg))'
#' tldr_explain(code)
#'
#' # Explain code from a file
#' tldr_explain("path/to/script.R")
#'
#' # Different detail levels
#' tldr_explain(code, detail_level = "basic")
#' tldr_explain(code, detail_level = "intermediate")
#' tldr_explain(code, detail_level = "advanced")
#'
#' # Line-by-line explanation
#' tldr_explain(code, line_by_line = TRUE)
#'
#' # Explain specific lines
#' tldr_explain(code, specific_lines = "2-3")
#'
#' # Use context awareness for more relevant explanations
#' library(dplyr)
#' data <- mtcars
#' tldr_explain(code, context_aware = TRUE)
#'
#' # Apply a character voice
#' tldr_explain(code, voice = "enthusiastic_explorer")
#' }
tldr_explain <- function(code, detail_level = "basic", highlight = TRUE, line_by_line = FALSE,
                         specific_lines = NULL, context_aware = FALSE, voice = NULL,
                         refresh = FALSE, provider = NULL) {
  # Validate input
  if (missing(code) || is.null(code)) {
    stop("Please provide code to explain (as a string or file path)")
  }
  
  # Check if code is a file path
  if (length(code) == 1 && file.exists(code) && grepl("\\.[rR]$", code)) {
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Reading code from file: ", code)
    }
    # Read the file
    code_content <- readLines(code, warn = FALSE)
    code <- paste(code_content, collapse = "\n")
    file_path <- code
  } else {
    file_path <- NULL
  }
  
  # Validate detail_level
  valid_detail_levels <- c("basic", "intermediate", "advanced")
  if (!detail_level %in% valid_detail_levels) {
    warning("Invalid detail_level: '", detail_level, 
            "'. Using 'basic' instead. Valid options are: ", 
            paste(valid_detail_levels, collapse = ", "))
    detail_level <- "basic"
  }
  
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
  
  # Generate a unique cache key based on the code and options
  cache_key <- digest::digest(
    list(
      code = code,
      detail_level = detail_level,
      highlight = highlight,
      line_by_line = line_by_line,
      specific_lines = specific_lines,
      context_aware = context_aware,
      provider = selected_provider
    ),
    algo = "sha256"
  )
  
  # Create cache path
  cache_dir <- get_config("cache_dir")
  cache_path <- file.path(cache_dir, paste0("explain_", cache_key, ".rds"))
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Code length: ", nchar(code))
    message("DEBUG: Detail level: ", detail_level)
    message("DEBUG: Line by line: ", line_by_line)
    message("DEBUG: Specific lines: ", specific_lines %||% "None")
    message("DEBUG: Context aware: ", context_aware)
    message("DEBUG: Provider: ", selected_provider)
    message("DEBUG: Cache path: ", cache_path)
  }
  
  # Check for cached response
  if (!refresh && !context_aware && file.exists(cache_path)) {
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
      
      print_explanation_response(response, code, detail_level, highlight, line_by_line,
                               specific_lines, selected_provider, voice)
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
      
      print_explanation_response(response, code, detail_level, highlight, line_by_line,
                              specific_lines, selected_provider, voice)
      return(invisible(response))
    }
    # Otherwise continue to get a fresh response
  } else if ((refresh || context_aware) && get_config("offline_mode", default = FALSE)) {
    if (context_aware) {
      stop("Cannot use context awareness in offline mode. Disable offline mode first with tldr_offline(FALSE).")
    } else {
      stop("Cannot refresh in offline mode. Disable offline mode first with tldr_offline(FALSE).")
    }
  }
  
  # Handle contextual awareness
  context_data <- NULL
  if (context_aware) {
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
    
    # Format context data for prompt
    context_data <- context_analyzer$format_context_for_prompt()
    
    # Add understanding of specific variables and functions used in the code
    code_vars <- extract_variables_from_code(code)
    code_funcs <- extract_functions_from_code(code)
    
    if (length(code_vars) > 0 || length(code_funcs) > 0) {
      context_data <- paste0(
        context_data,
        "\n\nCODE SPECIFIC CONTEXT:\n",
        if (length(code_vars) > 0) paste0("Variables in code: ", paste(code_vars, collapse = ", "), "\n") else "",
        if (length(code_funcs) > 0) paste0("Functions in code: ", paste(code_funcs, collapse = ", "), "\n") else ""
      )
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Context data generated successfully")
    }
  }
  
  # Build the prompt
  prompt <- build_explanation_prompt(code, detail_level, highlight, line_by_line, 
                                  specific_lines, context_data, file_path)
  
  # Get API response in offline mode?
  if (get_config("offline_mode", default = FALSE) && !file.exists(cache_path)) {
    stop("Code explanation not cached and offline mode is enabled. Disable offline mode to fetch response.")
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
  if (context_aware) {
    attr(response_obj, "context_aware") <- TRUE
  }
  
  # Cache the response
  if (get_config("cache_enabled", default = TRUE) && !context_aware) {
    if (!is.null(response_obj) && !grepl("\\{\\{CODE\\}\\}", response_obj, fixed = TRUE) &&
        nchar(response_obj) > 50) {
      # Ensure cache directory exists
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      
      saveRDS(response_obj, cache_path)
    }
  }
  
  # Print formatted response
  print_explanation_response(response_obj, code, detail_level, highlight, line_by_line,
                           specific_lines, selected_provider, voice)
  
  invisible(response_obj)
}

#' Build the prompt for code explanation
#'
#' @param code The code to explain
#' @param detail_level The desired detail level
#' @param highlight Whether to include syntax highlighting
#' @param line_by_line Whether to provide a line-by-line explanation
#' @param specific_lines Specific lines to explain
#' @param context_data Optional context data from the context analyzer
#' @param file_path Optional file path if code was read from a file
#'
#' @return Character string containing the prompt
#' @keywords internal
build_explanation_prompt <- function(code, detail_level, highlight, line_by_line, 
                                   specific_lines, context_data = NULL, file_path = NULL) {
  # Get the explanation template
  template <- get_explanation_template()
  
  # Replace placeholders with actual values
  template <- gsub("{{CODE}}", code, template, fixed = TRUE)
  template <- gsub("{{DETAIL_LEVEL}}", detail_level, template, fixed = TRUE)
  
  # Handle highlight flag
  highlight_str <- ifelse(highlight, "YES", "NO")
  template <- gsub("{{HIGHLIGHT}}", highlight_str, template, fixed = TRUE)
  
  # Handle line-by-line flag
  line_by_line_str <- ifelse(line_by_line, "YES", "NO")
  template <- gsub("{{LINE_BY_LINE}}", line_by_line_str, template, fixed = TRUE)
  
  # Handle specific lines
  if (!is.null(specific_lines) && nchar(specific_lines) > 0) {
    template <- gsub("{{SPECIFIC_LINES}}", specific_lines, template, fixed = TRUE)
    template <- gsub("{{HAS_SPECIFIC_LINES}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{SPECIFIC_LINES}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_SPECIFIC_LINES}}", "NO", template, fixed = TRUE)
  }
  
  # Handle file information
  if (!is.null(file_path)) {
    file_info <- paste0("This code was read from the file: ", file_path)
    template <- gsub("{{FILE_INFO}}", file_info, template, fixed = TRUE)
    template <- gsub("{{HAS_FILE_INFO}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{FILE_INFO}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_FILE_INFO}}", "NO", template, fixed = TRUE)
  }
  
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

#' Get the template for code explanation
#'
#' @return Character string containing the explanation template
#' @keywords internal
get_explanation_template <- function() {
  return('
You are tldrAI, a tool that provides concise, practical explanations of R code.
Your goal is to analyze and explain the provided R code in a way that is helpful, educational, and actionable.

Here is the R code to explain:
```r
{{CODE}}
```

CODE SOURCE INFO: {{HAS_FILE_INFO}}
{{FILE_INFO}}

Detail level requested: {{DETAIL_LEVEL}}
Include syntax highlighting: {{HIGHLIGHT}}
Line-by-line explanation: {{LINE_BY_LINE}}
Explain specific lines: {{HAS_SPECIFIC_LINES}}
{{SPECIFIC_LINES}}

Context awareness mode is set to: {{HAS_CONTEXT}}
{{CONTEXT_DATA}}

Please provide a response in the following format:

```
# Code Explanation

## Purpose
A concise description of what this code does and its overall purpose.

## Breakdown
{{#if LINE_BY_LINE is YES}}
A line-by-line explanation of the code:

Line X: Explanation of what this line does
Line Y-Z: Explanation of what this block does
...
{{else}}
A breakdown of the key components and operations in the code.
{{/if}}

{{#if DETAIL_LEVEL is "intermediate" or "advanced"}}
## Technical Details
More technical information about how the code works, including:
- Functions and methods used
- Data transformations
- Performance considerations
- Potential edge cases
{{/if}}

{{#if DETAIL_LEVEL is "advanced"}}
## Implementation Notes
Advanced information about the implementation:
- Memory usage
- Algorithmic complexity
- Alternative approaches
- Best practices
{{/if}}

## Key Concepts
List and briefly explain the main R concepts, functions, or packages used in this code.

{{#if HIGHLIGHT is YES}}
## Highlighted Code
```r
// Here include the original code with syntax highlighting and possibly inline comments
```
{{/if}}

{{#if SPECIFIC_LINES is specified}}
## Focus on Lines {{SPECIFIC_LINES}}
A detailed explanation of the specified lines and how they fit into the overall code.
{{/if}}
```

Detail Level Explanations:
- "basic": Focus on high-level purpose and general workflow, avoiding technical details that might confuse beginners.
- "intermediate": Include more technical details about packages, functions, and data transformations.
- "advanced": Provide in-depth analysis including performance considerations, edge cases, and implementation details.

Line-by-line mode should break down each significant line or logical block of code with its specific purpose.

If context awareness mode is YES:
1. Reference the user\'s environment variables, packages, and data structures in your explanation
2. Connect the code to the user\'s actual data frames and variables when relevant
3. Provide context on how this code relates to other functions or patterns in their environment
4. If the code uses variables that exist in the user\'s environment, incorporate information about them

If highlight mode is YES, include a syntax-highlighted version of the code, possibly with added comments.

If specific lines are requested, provide detailed focus on those particular lines while still explaining the overall context.

Your explanation should be:
1. Clear and concise
2. Accurate and technically sound
3. Appropriate for the requested detail level
4. Well-structured with logical organization
5. Free of jargon unless necessary (and explained when used)
6. Practical, focusing on how the code works and what it accomplishes
')
}

#' Print formatted explanation response
#'
#' @param response The raw response from the AI
#' @param code The original code
#' @param detail_level The detail level requested
#' @param highlight Whether highlighting was requested
#' @param line_by_line Whether line-by-line explanation was requested
#' @param specific_lines Specific lines requested for explanation
#' @param provider The LLM provider that generated the response
#' @param voice The character voice used (if any)
#'
#' @return Invisibly returns NULL
#' @keywords internal
print_explanation_response <- function(response, code, detail_level, highlight, line_by_line,
                                      specific_lines, provider = NULL, voice = NULL) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Printing explanation response")
    message("DEBUG: Raw response length: ", nchar(response))
  }
  
  # Extract the code block content
  content <- extract_code_block(response)
  
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Extracted content length: ", nchar(content))
  }
  
  # Traditional console output format
  cat("\n")
  cli::cli_h1("tldrAI: Code Explanation")
  
  # Special pre-processing for code explanations to fix formatting issues
  if (grepl("## Key Concepts", content, fixed = TRUE)) {
    # Fix potential issues with NA values in key concepts section
    content <- gsub("(- [^:]+:[^\\n]+): NA", "\\1", content)
  }
  
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

#' Extract variables from R code
#'
#' @param code The R code to analyze
#' @return Character vector of variable names
#' @keywords internal
extract_variables_from_code <- function(code) {
  # This is a simplified implementation - a more robust approach would use the codetools package
  # Parse the code into an expression
  tryCatch({
    exp <- parse(text = code)
    
    # Get all symbols in the expression
    symbols <- all.names(exp, unique = TRUE)
    
    # Filter out built-in functions and operators
    builtins <- c(
      names(.BaseNamespaceEnv), names(baseenv()), 
      "%>%", "+", "-", "*", "/", "^", "=", "<-", "->", "==", "!=", "<", ">", "<=", ">=",
      "!", "&", "|", "&&", "||", ":", "::", ":::", "$", "[[", "[", "@"
    )
    
    # Remove numbers
    symbols <- symbols[!grepl("^[0-9.]+$", symbols)]
    
    # Remove built-ins and common tokens
    symbols <- setdiff(symbols, builtins)
    
    # Return unique variable names
    unique(symbols)
  }, error = function(e) {
    # If parsing fails, return empty vector
    character(0)
  })
}

#' Extract function calls from R code
#'
#' @param code The R code to analyze
#' @return Character vector of function names
#' @keywords internal
extract_functions_from_code <- function(code) {
  # This is a simplified implementation - a more robust approach would use code analysis packages
  # Look for function-like patterns (name followed by opening parenthesis)
  tryCatch({
    matches <- regmatches(code, gregexpr("\\b[A-Za-z0-9_.]+\\s*\\(", code))
    funcs <- unlist(matches)
    
    # Clean up to get just the function names
    funcs <- gsub("\\s*\\($", "", funcs)
    
    # Filter out common control structures
    control_structures <- c("if", "for", "while", "function", "switch", "try")
    funcs <- funcs[!funcs %in% control_structures]
    
    # Return unique function names
    unique(funcs)
  }, error = function(e) {
    # If parsing fails, return empty vector
    character(0)
  })
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}