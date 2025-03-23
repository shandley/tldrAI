#' Summarize R scripts with AI-powered insights
#'
#' This function analyzes one or more R scripts and generates summaries of their purpose,
#' structure, and functionality. It uses large language models to provide intelligent
#' insights about the code, with different focus options to highlight specific aspects.
#'
#' @param files Character vector of file paths to R scripts. Must be valid R (.R) files.
#' @param focus Character string specifying the summary focus:
#'        \itemize{
#'          \item "general" - Overall script purpose and functionality (default)
#'          \item "functions" - Focus on functions, their purposes and relationships
#'          \item "data_flow" - Focus on data transformations and data flow
#'          \item "packages" - Focus on package dependencies and usage patterns
#'          \item "workflow" - Focus on the execution flow and computational steps
#'        }
#' @param detail Character string specifying the level of detail:
#'        \itemize{
#'          \item "brief" - Concise summary with essential information
#'          \item "standard" - Balanced detail with key insights (default)
#'          \item "comprehensive" - In-depth analysis with thorough explanations
#'        }
#' @param visualize Logical indicating whether to include a visualization of script structure.
#'        Default is FALSE.
#' @param vis_type Character string specifying visualization type (if visualize=TRUE):
#'        \itemize{
#'          \item "dependency" - Visualize dependencies between functions (default)
#'          \item "flow" - Visualize the execution flow
#'          \item "package" - Visualize package dependencies
#'          \item "structure" - Visualize script structure
#'        }
#' @param context Logical indicating whether to use context awareness to include
#'        information from the current R environment. Default is TRUE.
#' @param voice Character string specifying the character voice to use. Default is NULL
#'        (uses the configured default). See tldr_list_voices() for available options.
#' @param provider Character string specifying which LLM provider to use.
#'        Options: "claude" (default) or "openai". Requires API key to be configured.
#' @param refresh Logical indicating whether to ignore cached results and generate a fresh
#'        response. Default is FALSE.
#' @param output Character string specifying output format:
#'        \itemize{
#'          \item "console" - Formatted console output with color (default)
#'          \item "markdown" - Markdown text format for documents
#'        }
#'
#' @return Invisibly returns a list with the summary details. The list contains:
#'         \itemize{
#'           \item "summary": The generated summary text
#'           \item "files": The files that were analyzed
#'           \item "focus": The focus that was used
#'           \item "provider": The LLM provider that was used
#'           \item "visualization": The visualization object (if requested)
#'         }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Summarize a single script
#' tldr_summarize("path/to/script.R")
#'
#' # Summarize multiple scripts
#' tldr_summarize(c("script1.R", "script2.R", "script3.R"))
#'
#' # Different summary focuses
#' tldr_summarize("analysis.R", focus = "data_flow")
#' tldr_summarize("utils.R", focus = "functions")
#' tldr_summarize("pipeline.R", focus = "workflow")
#'
#' # Different detail levels
#' tldr_summarize("script.R", detail = "brief")
#' tldr_summarize("script.R", detail = "comprehensive")
#'
#' # Include visualization
#' tldr_summarize("script.R", visualize = TRUE)
#' tldr_summarize("script.R", visualize = TRUE, vis_type = "flow")
#'
#' # Use a character voice
#' tldr_summarize("script.R", voice = "enthusiastic_explorer")
#'
#' # Output as markdown
#' tldr_summarize("script.R", output = "markdown")
#' }
tldr_summarize <- function(files, focus = "general", detail = "standard",
                           visualize = FALSE, vis_type = "dependency",
                           context = TRUE, voice = NULL, provider = NULL,
                           refresh = FALSE, output = "console") {
  # Input validation
  if (missing(files) || length(files) == 0) {
    stop("Please provide at least one R script file path")
  }
  
  # Check if files exist and have .R extension
  invalid_files <- character(0)
  non_r_files <- character(0)
  
  for (file in files) {
    if (!file.exists(file)) {
      invalid_files <- c(invalid_files, file)
    } else if (!grepl("\\.R$|\\.r$", file)) {
      non_r_files <- c(non_r_files, file)
    }
  }
  
  if (length(invalid_files) > 0) {
    stop("The following files do not exist: ", paste(invalid_files, collapse = ", "))
  }
  
  if (length(non_r_files) > 0) {
    warning("The following files may not be R scripts: ", paste(non_r_files, collapse = ", "),
            "\nContinuing anyway, but results may be unexpected.")
  }
  
  # Validate focus
  valid_focuses <- c("general", "functions", "data_flow", "packages", "workflow")
  if (!focus %in% valid_focuses) {
    warning("Invalid focus: '", focus, 
            "'. Using 'general' instead. Valid options are: ", 
            paste(valid_focuses, collapse = ", "))
    focus <- "general"
  }
  
  # Validate detail
  valid_details <- c("brief", "standard", "comprehensive")
  if (!detail %in% valid_details) {
    warning("Invalid detail: '", detail, 
            "'. Using 'standard' instead. Valid options are: ", 
            paste(valid_details, collapse = ", "))
    detail <- "standard"
  }
  
  # Validate vis_type if visualization is requested
  if (visualize) {
    valid_vis_types <- c("dependency", "flow", "package", "structure")
    if (!vis_type %in% valid_vis_types) {
      warning("Invalid visualization type: '", vis_type, 
              "'. Using 'dependency' instead. Valid options are: ", 
              paste(valid_vis_types, collapse = ", "))
      vis_type <- "dependency"
    }
  }
  
  # Validate output
  valid_outputs <- c("console", "markdown")
  if (!output %in% valid_outputs) {
    warning("Invalid output: '", output, 
            "'. Using 'console' instead. Valid options are: ", 
            paste(valid_outputs, collapse = ", "))
    output <- "console"
  }
  
  # Use default voice if NULL
  if (is.null(voice)) voice <- get_config("character_voice", default = "none")
  
  # Set refresh mode temporarily in the config
  if (refresh) {
    old_refresh_mode <- get_config("refresh_mode", default = FALSE)
    tldr_config(refresh_mode = TRUE)
    on.exit(tldr_config(refresh_mode = old_refresh_mode))
  }
  
  # Determine which provider to use
  selected_provider <- provider %||% get_config("provider", default = "claude")
  
  # Read all script files
  scripts_content <- list()
  for (file in files) {
    scripts_content[[file]] <- paste(readLines(file, warn = FALSE), collapse = "\n")
  }
  
  # Generate a unique cache key based on files content and options
  cache_key <- digest::digest(
    list(
      files = files,
      files_content = scripts_content,
      focus = focus,
      detail = detail,
      context = context,
      provider = selected_provider
    ),
    algo = "sha256"
  )
  
  # Create cache path
  cache_dir <- get_config("cache_dir")
  cache_path <- file.path(cache_dir, paste0("summarize_script_", cache_key, ".rds"))
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Number of files: ", length(files))
    message("DEBUG: Focus: ", focus)
    message("DEBUG: Detail: ", detail)
    message("DEBUG: Context awareness: ", context)
    message("DEBUG: Provider: ", selected_provider)
    message("DEBUG: Cache path: ", cache_path)
  }
  
  # Initialize result object
  result <- list(
    summary = NULL,
    files = files,
    focus = focus,
    provider = selected_provider,
    visualization = NULL
  )
  
  # Check for cached response
  if (!refresh && file.exists(cache_path)) {
    # Check if cache is expired
    cache_ttl <- get_config("cache_ttl", default = 30)
    file_time <- file.info(cache_path)$mtime
    now <- Sys.time()
    
    if (difftime(now, file_time, units = "days") <= cache_ttl) {
      result <- readRDS(cache_path)
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        result$summary <- apply_character_voice(result$summary, voice)
      }
      
      # Print formatted response
      print_script_summary(result, focus, detail, files, output, voice, selected_provider)
      
      # Generate visualization if requested
      if (visualize) {
        vis_result <- generate_script_visualization(files, vis_type)
        result$visualization <- vis_result
      }
      
      return(invisible(result))
    } else if (get_config("offline_mode", default = FALSE)) {
      # In offline mode, use expired cache anyway
      result <- readRDS(cache_path)
      message("Using expired cached response (offline mode)")
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        result$summary <- apply_character_voice(result$summary, voice)
      }
      
      # Print formatted response
      print_script_summary(result, focus, detail, files, output, voice, selected_provider)
      
      # Generate visualization if requested
      if (visualize) {
        vis_result <- generate_script_visualization(files, vis_type)
        result$visualization <- vis_result
      }
      
      return(invisible(result))
    }
    # Otherwise continue to get a fresh response
  } else if (refresh && get_config("offline_mode", default = FALSE)) {
    stop("Cannot refresh in offline mode. Disable offline mode first with tldr_offline(FALSE).")
  }
  
  # Extract context if requested
  context_info <- NULL
  if (context) {
    context_info <- get_context_for_scripts(files)
  }
  
  # Build the prompt
  prompt <- build_script_summary_prompt(scripts_content, focus, detail, context_info)
  
  # Get API response in offline mode?
  if (get_config("offline_mode", default = FALSE) && !file.exists(cache_path)) {
    stop("Script summary not cached and offline mode is enabled. Disable offline mode to fetch response.")
  }
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generated prompt (truncated):")
    message(substr(prompt, 1, 300), "...")
  }
  
  # Get API response
  response <- get_ai_response(prompt, provider_override = selected_provider)
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: API Response (first 100 chars): ", substr(response, 1, 100))
  }
  
  # Store response in result
  result$summary <- response
  
  # Apply character voice transformation if selected
  if (voice != "none") {
    result$summary <- apply_character_voice(result$summary, voice)
  }
  
  # Cache the response
  if (get_config("cache_enabled", default = TRUE)) {
    if (!is.null(response) && nchar(response) > 50) {
      # Ensure cache directory exists
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      
      saveRDS(result, cache_path)
    }
  }
  
  # Generate visualization if requested
  if (visualize) {
    vis_result <- generate_script_visualization(files, vis_type)
    result$visualization <- vis_result
  }
  
  # Print formatted response
  print_script_summary(result, focus, detail, files, output, voice, selected_provider)
  
  invisible(result)
}

#' Build prompt for R script summarization
#'
#' @param scripts_content List of script content keyed by file path
#' @param focus The summary focus
#' @param detail The level of detail
#' @param context_info Optional context information
#'
#' @return Character string containing the prompt
#' @keywords internal
build_script_summary_prompt <- function(scripts_content, focus, detail, context_info = NULL) {
  # Get the script summary template
  template <- get_script_summary_template()
  
  # Create formatted script content
  scripts_text <- character(0)
  for (file_path in names(scripts_content)) {
    file_content <- scripts_content[[file_path]]
    scripts_text <- c(scripts_text, 
                     paste0("FILE: ", file_path, "\n```r\n", file_content, "\n```\n"))
  }
  
  all_scripts <- paste(scripts_text, collapse = "\n\n")
  
  # Replace placeholders with actual values
  template <- gsub("{{SCRIPTS}}", all_scripts, template, fixed = TRUE)
  template <- gsub("{{FOCUS}}", focus, template, fixed = TRUE)
  template <- gsub("{{DETAIL}}", detail, template, fixed = TRUE)
  
  # Handle context information
  if (!is.null(context_info) && nchar(context_info) > 0) {
    template <- gsub("{{CONTEXT_INFO}}", context_info, template, fixed = TRUE)
    template <- gsub("{{HAS_CONTEXT}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{CONTEXT_INFO}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_CONTEXT}}", "NO", template, fixed = TRUE)
  }
  
  # Replace number of scripts
  template <- gsub("{{NUM_SCRIPTS}}", length(scripts_content), template, fixed = TRUE)
  
  template
}

#' Get script summary template
#'
#' @return Character string containing the template
#' @keywords internal
get_script_summary_template <- function() {
  return('
You are tldrAI, an advanced R code analysis tool that provides insightful summaries of R scripts.
Your task is to analyze the provided R script(s) and generate a summary according to the specified parameters.

------------------------------
SCRIPTS TO ANALYZE ({{NUM_SCRIPTS}} files):
{{SCRIPTS}}
------------------------------

CONTEXT INFORMATION: {{HAS_CONTEXT}}
{{CONTEXT_INFO}}

FOCUS: {{FOCUS}}
DETAIL LEVEL: {{DETAIL}}

Please provide a summary with the following characteristics:

1. FOCUS:
   - "general": Overall script purpose and functionality
   - "functions": Focus on functions, their purposes and relationships
   - "data_flow": Focus on data transformations and data flow
   - "packages": Focus on package dependencies and usage patterns
   - "workflow": Focus on the execution flow and computational steps

2. DETAIL LEVEL:
   - "brief": Concise summary with essential information
   - "standard": Balanced detail with key insights
   - "comprehensive": In-depth analysis with thorough explanations

Your summary should:
1. Accurately describe what the script(s) do and their purpose
2. Highlight key functions, packages, and data operations
3. Be well-structured and easy to understand
4. Match the requested focus and detail level
5. Include examples of important code patterns when relevant

If analyzing multiple scripts, also:
1. Explain how the scripts are related or work together
2. Identify shared functionality or dependencies
3. Describe the overall workflow across scripts

Format your response as follows:

```
# R Script Summary

## Overview
[A concise overview of what the script(s) do and their purpose]

## Key Components
[List of the main components, functions, or operations in the script(s)]

## Functionality Details
[Detailed explanation focused on the requested aspect ({{FOCUS}})]

## Package Dependencies
[Key packages used and their purpose]

## Data Flow
[Description of how data is processed, transformed, and output]

## Execution Flow
[The sequence of operations and control flow]

## Key Insights
[Important observations or suggestions about the code]
```
')
}

#' Get context information for script analysis
#'
#' @param files Character vector of file paths
#'
#' @return Character string with context information
#' @keywords internal
get_context_for_scripts <- function(files) {
  # Skip if context analysis is not allowed
  if (!get_config("context_awareness_enabled", default = TRUE)) {
    return(NULL)
  }
  
  # Create a ContextAnalyzer instance
  analyzer <- ContextAnalyzer$new()
  
  # Get environment information
  env_info <- analyzer$get_environment_info()
  packages_info <- analyzer$get_packages_info()
  data_frames_info <- analyzer$get_data_frames_info()
  
  # Extract functions from script files
  script_functions <- list()
  for (file in files) {
    script_content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    extracted_functions <- extract_functions_from_code(script_content)
    script_functions[[file]] <- extracted_functions
  }
  
  # Check if any of the functions from scripts are used in the history
  history_info <- analyzer$check_history_for_functions(unique(unlist(script_functions)))
  
  # Build context information
  context <- character(0)
  
  # Add environment info
  context <- c(context, "R ENVIRONMENT:", env_info)
  
  # Add packages info if available
  if (length(packages_info) > 0) {
    context <- c(context, "", "LOADED PACKAGES:", paste(packages_info, collapse = "\n"))
  }
  
  # Add data frames info if available
  if (length(data_frames_info) > 0) {
    context <- c(context, "", "DATA FRAMES IN ENVIRONMENT:", paste(data_frames_info, collapse = "\n"))
  }
  
  # Add history info if available
  if (length(history_info) > 0) {
    context <- c(context, "", "USAGE HISTORY:", paste(history_info, collapse = "\n"))
  }
  
  # Combine all context info
  if (length(context) > 0) {
    return(paste(context, collapse = "\n"))
  } else {
    return(NULL)
  }
}

#' Generate visualization for R script structure
#'
#' @param files Character vector of file paths
#' @param vis_type The type of visualization to generate
#'
#' @return A visualization object (depends on the visualization type)
#' @keywords internal
generate_script_visualization <- function(files, vis_type = "dependency") {
  # Create a visualization handler
  viz_handler <- VisualizationHandler$new()
  
  # Choose visualization type based on vis_type parameter
  if (vis_type == "dependency") {
    return(viz_handler$generate_script_dependency_visualization(files))
  } else if (vis_type == "flow") {
    return(viz_handler$generate_script_flow_visualization(files))
  } else if (vis_type == "package") {
    return(viz_handler$generate_script_package_visualization(files))
  } else if (vis_type == "structure") {
    return(viz_handler$generate_script_structure_visualization(files))
  } else {
    warning("Unsupported visualization type: ", vis_type)
    return(NULL)
  }
}

#' Print script summary
#'
#' @param result List containing the summary result
#' @param focus The focus that was used
#' @param detail The detail level that was used
#' @param files The files that were analyzed
#' @param output The output format ("console" or "markdown")
#' @param voice The character voice used (if any)
#' @param provider The LLM provider that was used
#'
#' @return Invisibly returns NULL
#' @keywords internal
print_script_summary <- function(result, focus, detail, files, output = "console", 
                               voice = NULL, provider = NULL) {
  # Get the summary from the result
  summary_text <- result$summary
  
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Printing script summary")
    message("DEBUG: Summary length: ", nchar(summary_text))
  }
  
  # Extract the code block content
  content <- extract_code_block(summary_text)
  
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Extracted content length: ", nchar(content))
  }
  
  if (output == "console") {
    # Console output format
    cat("\n")
    cli::cli_h1("tldrAI: R Script Summary")
    
    # Format the content
    formatted_content <- format_content(content)
    cat(formatted_content)
    
    # Add footer information
    cat("\n")
    provider_name <- provider %||% result$provider %||% "claude"
    provider_display <- ifelse(provider_name == "claude", "Claude's API", "OpenAI's API")
    
    cli::cli_text("{.emph Generated by tldrAI using ", provider_display, "}")
    
    # Add file information
    if (length(files) == 1) {
      cli::cli_text("{.emph Analyzed file: ", files, "}")
    } else {
      cli::cli_text("{.emph Analyzed ", length(files), " files}")
      # First few files with ellipsis if there are many
      max_files_to_show <- 3
      if (length(files) <= max_files_to_show) {
        file_list <- paste(files, collapse = ", ")
      } else {
        file_list <- paste(c(files[1:max_files_to_show], "..."), collapse = ", ")
      }
      cli::cli_text("{.emph Files: ", file_list, "}")
    }
    
    # Add character voice info if used
    voice_name <- voice %||% "none"
    if (voice_name != "none") {
      # Format voice name for display (replace underscores with spaces, capitalize words)
      display_voice <- gsub("_", " ", voice_name)
      display_voice <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", display_voice, perl = TRUE)
      
      cli::cli_text("{.emph Character voice: ", display_voice, "}")
    }
    
    # Add summary parameters
    params <- c()
    params <- c(params, paste0("Focus: ", toupper(substr(focus, 1, 1)), substr(focus, 2, nchar(focus))))
    params <- c(params, paste0("Detail: ", toupper(substr(detail, 1, 1)), substr(detail, 2, nchar(detail))))
    
    cli::cli_text("{.emph Parameters: ", paste(params, collapse = ", "), "}")
    
    cat("\n")
  } else if (output == "markdown") {
    # Markdown output format
    cat(content)
    
    # Add metadata as YAML frontmatter
    cat("\n\n---\n")
    cat("generated_by: tldrAI\n")
    cat("provider: ", provider %||% result$provider %||% "claude", "\n", sep = "")
    cat("files_analyzed: ", paste(files, collapse = ", "), "\n", sep = "")
    cat("focus: ", focus, "\n", sep = "")
    cat("detail: ", detail, "\n", sep = "")
    if (voice != "none") {
      cat("voice: ", voice, "\n", sep = "")
    }
    cat("---\n")
  }
  
  invisible(NULL)
}

# Helper function to extract content from code blocks
extract_code_block <- function(text) {
  if (is.null(text)) return("")
  
  # Look for markdown code block format
  matches <- regmatches(text, gregexpr("```(?:markdown)?\\n([\\s\\S]*?)\\n```", text, perl = TRUE))
  
  if (length(matches) > 0 && length(matches[[1]]) > 0) {
    # Extract content from the first code block
    block <- matches[[1]][1]
    # Remove the opening and closing ```
    content <- gsub("```(?:markdown)?\\n|\\n```$", "", block, perl = TRUE)
    return(content)
  }
  
  # If no code block found, return the original text
  return(text)
}

# Helper function to format content for display
format_content <- function(content) {
  if (is.null(content) || content == "") return("")
  
  # Process headings
  content <- gsub("^# (.+)$", function(x) cli::cli_h1(sub("^# ", "", x)), content, perl = TRUE)
  content <- gsub("^## (.+)$", function(x) cli::cli_h2(sub("^## ", "", x)), content, perl = TRUE)
  content <- gsub("^### (.+)$", function(x) cli::cli_h3(sub("^### ", "", x)), content, perl = TRUE)
  
  # Process code blocks within content (if they exist)
  content <- gsub("```r\\n([\\s\\S]*?)\\n```", function(x) {
    code <- gsub("```r\\n|\\n```", "", x)
    paste0("\n", crayon::silver(code), "\n")
  }, content, perl = TRUE)
  
  # Handle inline code
  content <- gsub("`([^`]+)`", function(x) {
    crayon::silver(gsub("`", "", x))
  }, content, perl = TRUE)
  
  # Handle bullet points
  content <- gsub("^- (.+)$", function(x) paste0("• ", sub("^- ", "", x)), content, perl = TRUE)
  
  # Handle numbered lists
  content <- gsub("^(\\d+)\\. (.+)$", function(x) {
    matches <- regmatches(x, regexec("^(\\d+)\\. (.+)$", x))[[1]]
    paste0(matches[2], ". ", matches[3])
  }, content, perl = TRUE)
  
  # Handle emphasis/bold
  content <- gsub("\\*\\*([^*]+)\\*\\*", function(x) {
    crayon::bold(gsub("\\*\\*", "", x))
  }, content, perl = TRUE)
  
  # Handle italics
  content <- gsub("\\*([^*]+)\\*", function(x) {
    crayon::italic(gsub("\\*", "", x))
  }, content, perl = TRUE)
  
  return(content)
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}