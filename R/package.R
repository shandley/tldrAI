#' Analyze an R package and provide a comprehensive overview
#'
#' This function generates an AI-powered analysis of an R package, including its functionality,
#' exported functions, datasets, and usage patterns. It provides a structured overview that
#' helps users understand package capabilities and common workflows.
#'
#' @param package_name Character string specifying the name of an R package to analyze.
#' @param focus Character string specifying the analysis focus. Options:
#'        \itemize{
#'          \item "overview": General package purpose and key functions (default)
#'          \item "functions": Detailed exploration of all exported functions
#'          \item "datasets": Description of included datasets
#'          \item "vignettes": Summary of package vignettes
#'          \item "dependencies": Analysis of package dependencies and their purposes
#'        }
#' @param interactive Logical indicating whether to create an interactive explorer
#'        for navigating through package components. Default is FALSE.
#' @param detail Character string specifying the level of detail. Options:
#'        \itemize{
#'          \item "brief": Quick overview with key functions only
#'          \item "standard": Balanced detail with main function categories (default)
#'          \item "comprehensive": Full details on all exported functions and features
#'        }
#' @param categories Logical indicating whether to categorize functions by purpose.
#'        Default is TRUE.
#' @param examples Integer specifying the number of workflow examples to provide.
#'        Default is 2.
#' @param voice Character string specifying the character voice to use. Use
#'        \code{\link{tldr_list_voices}} to see available options. Default is NULL (uses the 
#'        configured default).
#' @param provider Character string specifying the LLM provider to use. Options are "claude" (default)
#'        or "openai". Requires API key to be configured for the selected provider.
#' @param refresh Logical indicating whether to ignore cached results and generate a fresh response.
#'        Default is FALSE.
#' @param output Character string specifying the output format. Options:
#'        \itemize{
#'          \item "console": Formatted console output (default)
#'          \item "markdown": Raw markdown text
#'          \item "html": HTML format (requires rmarkdown package)
#'        }
#'
#' @return Prints formatted package analysis to the console (or specified output format) and 
#'         invisibly returns the raw response as a character string. The returned object 
#'         has attributes that can be accessed with attr():
#'         \itemize{
#'           \item "provider": The LLM provider used ("claude" or "openai")
#'           \item "voice": The character voice used (if any)
#'           \item "package_name": The name of the analyzed package
#'           \item "focus": The analysis focus that was used
#'           \item "categories": Whether function categorization was performed
#'         }
#'
#' @seealso
#' \code{\link{tldr}} for explaining R functions
#' \code{\link{tldr_explain}} for explaining R code
#' \code{\link{tldr_error}} for explaining R errors
#' \code{\link{tldr_config}} for configuration options
#' \code{\link{tldr_list_voices}} for available character voices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic package overview
#' tldr_package("dplyr")                     # Overview of dplyr package
#' 
#' # Focus on specific aspects
#' tldr_package("ggplot2", focus = "functions")     # Functions in ggplot2
#' tldr_package("datasets", focus = "datasets")     # Datasets in the datasets package
#' tldr_package("knitr", focus = "vignettes")       # Vignettes in knitr
#' tldr_package("tidyr", focus = "dependencies")    # Dependencies of tidyr
#' 
#' # Varying levels of detail
#' tldr_package("lubridate", detail = "brief")            # Brief overview
#' tldr_package("purrr", detail = "comprehensive")        # Comprehensive analysis
#' 
#' # Interactive exploration
#' tldr_package("shiny", interactive = TRUE)        # Interactive explorer
#' 
#' # More workflow examples
#' tldr_package("tidyr", examples = 5)              # 5 workflow examples
#' 
#' # Different output formats
#' tldr_package("readr", output = "markdown")       # Raw markdown
#' tldr_package("forcats", output = "html")         # HTML output
#' 
#' # Character voices and providers
#' tldr_package("tibble", voice = "enthusiastic_explorer")  # Excited voice
#' tldr_package("stringr", provider = "openai")             # Using OpenAI
#' }
tldr_package <- function(package_name, focus = "overview", interactive = FALSE, 
                        detail = "standard", categories = TRUE, examples = 2,
                        voice = NULL, provider = NULL, refresh = FALSE,
                        output = "console") {
  # Validate input
  if (!is.character(package_name) || length(package_name) != 1) {
    stop("package_name must be a single character string")
  }
  
  # Validate focus parameter
  valid_focus <- c("overview", "functions", "datasets", "vignettes", "dependencies")
  if (!(focus %in% valid_focus)) {
    stop("focus must be one of: ", paste(valid_focus, collapse = ", "))
  }
  
  # Validate detail parameter
  valid_detail <- c("brief", "standard", "comprehensive")
  if (!(detail %in% valid_detail)) {
    stop("detail must be one of: ", paste(valid_detail, collapse = ", "))
  }
  
  # Validate output parameter
  valid_output <- c("console", "markdown", "html")
  if (!(output %in% valid_output)) {
    stop("output must be one of: ", paste(valid_output, collapse = ", "))
  }
  
  # Check if html output requires rmarkdown
  if (output == "html" && !requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("HTML output requires the 'rmarkdown' package. Install with: install.packages('rmarkdown')")
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
  
  # Generate a unique cache key
  cache_key <- digest::digest(
    list(
      package_name = package_name,
      focus = focus,
      detail = detail,
      categories = categories,
      examples = examples,
      provider = selected_provider
    ),
    algo = "sha256"
  )
  
  # Create cache path
  cache_dir <- get_config("cache_dir")
  cache_path <- file.path(cache_dir, paste0("package_", cache_key, ".rds"))
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Package name: ", package_name)
    message("DEBUG: Focus: ", focus)
    message("DEBUG: Detail level: ", detail)
    message("DEBUG: Categories: ", categories)
    message("DEBUG: Examples requested: ", examples)
    message("DEBUG: Output format: ", output)
    message("DEBUG: Provider: ", selected_provider)
    message("DEBUG: Cache path: ", cache_path)
  }
  
  # Check for cached response
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
      
      print_package_response(response, package_name, focus, categories, 
                           examples, selected_provider, voice, output, interactive)
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
      
      print_package_response(response, package_name, focus, categories,
                           examples, selected_provider, voice, output, interactive)
      return(invisible(response))
    }
    # Otherwise continue to get a fresh response
  } else if (refresh && get_config("offline_mode", default = FALSE)) {
    stop("Cannot refresh in offline mode. Disable offline mode first with tldr_offline(FALSE).")
  }
  
  # Check if package is installed
  package_installed <- package_name %in% rownames(installed.packages())
  
  # Get package metadata
  pkg_metadata <- analyze_package(package_name, package_installed)
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Package installed: ", package_installed)
    message("DEBUG: Package has ", length(pkg_metadata$exports), " exports")
    message("DEBUG: Package has ", length(pkg_metadata$datasets), " datasets")
    message("DEBUG: Package has ", length(pkg_metadata$vignettes), " vignettes")
  }
  
  # Build the prompt
  prompt <- build_package_prompt(package_name, pkg_metadata, focus, detail, 
                               categories, examples, package_installed)
  
  # Get API response in offline mode?
  if (get_config("offline_mode", default = FALSE) && !file.exists(cache_path)) {
    stop("Package analysis not cached and offline mode is enabled. Disable offline mode to fetch response.")
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
  
  # Add package metadata as attributes
  attr(response_obj, "package_name") <- package_name
  attr(response_obj, "focus") <- focus
  attr(response_obj, "categories") <- categories
  
  # Cache the response
  if (get_config("cache_enabled", default = TRUE)) {
    if (!is.null(response_obj) && nchar(response_obj) > 50) {
      # Ensure cache directory exists
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      
      saveRDS(response_obj, cache_path)
    }
  }
  
  # Print formatted response
  print_package_response(response_obj, package_name, focus, categories,
                       examples, selected_provider, voice, output, interactive)
  
  invisible(response_obj)
}

#' Build the prompt for package analysis
#'
#' @param package_name Name of the package
#' @param pkg_metadata Package metadata from analyze_package()
#' @param focus Analysis focus ("overview", "functions", "datasets", "vignettes", "dependencies")
#' @param detail Detail level ("brief", "standard", "comprehensive")
#' @param categories Whether to categorize functions
#' @param examples Number of workflow examples to request
#' @param package_installed Whether the package is installed
#'
#' @return Character string containing the prompt
#' @keywords internal
build_package_prompt <- function(package_name, pkg_metadata, focus, detail, 
                               categories, examples, package_installed) {
  # Get the package analysis template
  if (!package_installed) {
    template <- get_missing_package_analysis_template()
    template <- gsub("{{PACKAGE_NAME}}", package_name, template, fixed = TRUE)
    template <- gsub("{{INSTALL_COMMAND}}", paste0('install.packages("', package_name, '")'), template, fixed = TRUE)
    return(template)
  }
  
  # Regular package analysis
  template <- get_package_analysis_template()
  
  # Replace placeholders with actual values
  template <- gsub("{{PACKAGE_NAME}}", package_name, template, fixed = TRUE)
  
  # Handle focus
  template <- gsub("{{FOCUS}}", focus, template, fixed = TRUE)
  
  # Handle detail level
  template <- gsub("{{DETAIL_LEVEL}}", detail, template, fixed = TRUE)
  
  # Handle categorization
  categories_str <- ifelse(categories, "YES", "NO")
  template <- gsub("{{CATEGORIZE}}", categories_str, template, fixed = TRUE)
  
  # Handle examples count
  template <- gsub("{{EXAMPLES_REQUESTED}}", as.character(examples), template, fixed = TRUE)
  
  # Add package metadata
  
  # Package description
  if (!is.null(pkg_metadata$description)) {
    template <- gsub("{{PACKAGE_DESCRIPTION}}", pkg_metadata$description, template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_DESCRIPTION}}", "No description available", template, fixed = TRUE)
  }
  
  # Package version
  if (!is.null(pkg_metadata$version)) {
    template <- gsub("{{PACKAGE_VERSION}}", pkg_metadata$version, template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_VERSION}}", "Unknown", template, fixed = TRUE)
  }
  
  # Package exports (functions)
  if (length(pkg_metadata$exports) > 0) {
    exports_str <- paste(pkg_metadata$exports, collapse = ", ")
    template <- gsub("{{PACKAGE_EXPORTS}}", exports_str, template, fixed = TRUE)
    template <- gsub("{{HAS_EXPORTS}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_EXPORTS}}", "None", template, fixed = TRUE)
    template <- gsub("{{HAS_EXPORTS}}", "NO", template, fixed = TRUE)
  }
  
  # Package datasets
  if (length(pkg_metadata$datasets) > 0) {
    datasets_str <- paste0(capture.output(print(pkg_metadata$datasets)), collapse = "\n")
    template <- gsub("{{PACKAGE_DATASETS}}", datasets_str, template, fixed = TRUE)
    template <- gsub("{{HAS_DATASETS}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_DATASETS}}", "None", template, fixed = TRUE)
    template <- gsub("{{HAS_DATASETS}}", "NO", template, fixed = TRUE)
  }
  
  # Package vignettes
  if (length(pkg_metadata$vignettes) > 0) {
    vignettes_str <- paste0(capture.output(print(pkg_metadata$vignettes)), collapse = "\n")
    template <- gsub("{{PACKAGE_VIGNETTES}}", vignettes_str, template, fixed = TRUE)
    template <- gsub("{{HAS_VIGNETTES}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_VIGNETTES}}", "None", template, fixed = TRUE)
    template <- gsub("{{HAS_VIGNETTES}}", "NO", template, fixed = TRUE)
  }
  
  # Package dependencies
  if (length(pkg_metadata$dependencies) > 0) {
    deps_str <- paste(pkg_metadata$dependencies, collapse = ", ")
    template <- gsub("{{PACKAGE_DEPENDENCIES}}", deps_str, template, fixed = TRUE)
    template <- gsub("{{HAS_DEPENDENCIES}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_DEPENDENCIES}}", "None", template, fixed = TRUE)
    template <- gsub("{{HAS_DEPENDENCIES}}", "NO", template, fixed = TRUE)
  }
  
  # Package imports
  if (length(pkg_metadata$imports) > 0) {
    imports_str <- paste(pkg_metadata$imports, collapse = ", ")
    template <- gsub("{{PACKAGE_IMPORTS}}", imports_str, template, fixed = TRUE)
    template <- gsub("{{HAS_IMPORTS}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_IMPORTS}}", "None", template, fixed = TRUE)
    template <- gsub("{{HAS_IMPORTS}}", "NO", template, fixed = TRUE)
  }
  
  # Package suggests
  if (length(pkg_metadata$suggests) > 0) {
    suggests_str <- paste(pkg_metadata$suggests, collapse = ", ")
    template <- gsub("{{PACKAGE_SUGGESTS}}", suggests_str, template, fixed = TRUE)
    template <- gsub("{{HAS_SUGGESTS}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{PACKAGE_SUGGESTS}}", "None", template, fixed = TRUE)
    template <- gsub("{{HAS_SUGGESTS}}", "NO", template, fixed = TRUE)
  }
  
  # Function documentation (for key functions)
  if (!is.null(pkg_metadata$function_docs) && length(pkg_metadata$function_docs) > 0) {
    # Limit to a reasonable number of functions to avoid exceeding token limits
    max_funcs <- 25  # Adjust based on testing
    
    if (length(pkg_metadata$function_docs) > max_funcs && detail != "comprehensive") {
      # For non-comprehensive analysis, limit the number of function docs
      func_subset <- pkg_metadata$function_docs[1:max_funcs]
      func_docs_str <- paste(
        paste0("Documentation for ", max_funcs, " out of ", 
               length(pkg_metadata$function_docs), " functions:"),
        paste0(func_subset, collapse = "\n\n---\n\n")
      )
    } else {
      func_docs_str <- paste0(pkg_metadata$function_docs, collapse = "\n\n---\n\n")
    }
    template <- gsub("{{FUNCTION_DOCUMENTATION}}", func_docs_str, template, fixed = TRUE)
    template <- gsub("{{HAS_FUNCTION_DOCS}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{FUNCTION_DOCUMENTATION}}", "No function documentation available", template, fixed = TRUE)
    template <- gsub("{{HAS_FUNCTION_DOCS}}", "NO", template, fixed = TRUE)
  }
  
  template
}

#' Get the template for package analysis
#'
#' @return Character string containing the package analysis template
#' @keywords internal
get_package_analysis_template <- function() {
  return('
You are tldrAI, a tool that provides concise, practical help for R packages.
Your goal is to create a helpful analysis of the R package "{{PACKAGE_NAME}}".

Here is detailed information about the package:
Package name: "{{PACKAGE_NAME}}"
Package version: {{PACKAGE_VERSION}}
Package description: {{PACKAGE_DESCRIPTION}}

Focus requested: {{FOCUS}}
Detail level requested: {{DETAIL_LEVEL}}
Function categorization requested: {{CATEGORIZE}}
Examples requested: {{EXAMPLES_REQUESTED}}

EXPORTED FUNCTIONS: {{HAS_EXPORTS}}
{{PACKAGE_EXPORTS}}

DATASETS: {{HAS_DATASETS}}
{{PACKAGE_DATASETS}}

VIGNETTES: {{HAS_VIGNETTES}}
{{PACKAGE_VIGNETTES}}

DEPENDENCIES: {{HAS_DEPENDENCIES}}
{{PACKAGE_DEPENDENCIES}}

IMPORTS: {{HAS_IMPORTS}}
{{PACKAGE_IMPORTS}}

SUGGESTS: {{HAS_SUGGESTS}}
{{PACKAGE_SUGGESTS}}

FUNCTION DOCUMENTATION: {{HAS_FUNCTION_DOCS}}
{{FUNCTION_DOCUMENTATION}}

Please provide a response in the following format, focusing on the "{{FOCUS}}" aspect of the package:

```
# {{PACKAGE_NAME}} Package

## Overview
A concise description of what this package does and its primary purpose. Mention the key problems it solves and its place in the R ecosystem.

## Installation
```r
install.packages("{{PACKAGE_NAME}}")

# Or development version (if applicable)
# remotes::install_github("author/{{PACKAGE_NAME}}")
```

{{#if FOCUS is "overview" or FOCUS is "functions"}}
## Key Functions
{{#if CATEGORIZE is YES}}
Functions grouped by purpose:

### Data Import/Export
- `function1()`: Brief description
- `function2()`: Brief description

### Data Manipulation
- `function3()`: Brief description
- `function4()`: Brief description

[Add more categories as appropriate for this package]
{{else}}
- `function1()`: Brief description
- `function2()`: Brief description
- `function3()`: Brief description
[Include the most important/frequently used functions]
{{/if}}
{{/if}}

{{#if FOCUS is "datasets"}}
## Included Datasets
- `dataset1`: Brief description [rows × columns]
- `dataset2`: Brief description [rows × columns]

### Example Dataset Structure
```r
str({{PACKAGE_NAME}}::dataset1)
```
{{/if}}

{{#if FOCUS is "vignettes"}}
## Available Vignettes
- "Vignette Title 1": Brief description of what this vignette covers
- "Vignette Title 2": Brief description of what this vignette covers

### Accessing Vignettes
```r
vignette("vignette_name", package = "{{PACKAGE_NAME}}")
# or
browseVignettes("{{PACKAGE_NAME}}")
```
{{/if}}

{{#if FOCUS is "dependencies"}}
## Dependencies
- Required packages: List with brief explanation of why each is needed
- Suggested packages: List with brief explanation of when you might need them

### Dependency Map
[Brief explanation of how these packages work together]
{{/if}}

## Usage Examples
{{EXAMPLES_REQUESTED}} practical workflow examples demonstrating common use cases:

### Example 1: [Brief title]
```r
library({{PACKAGE_NAME}})

# Example code
# ...

# Expected output or visualization
```

[Brief explanation of what this example demonstrates]

[Additional examples if requested...]

## Tips and Best Practices
- Practical advice for using the package effectively
- Common pitfalls to avoid
- Performance considerations
{{#if DETAIL_LEVEL is "comprehensive"}}
- Advanced usage patterns
{{/if}}

{{#if DETAIL_LEVEL is "comprehensive"}}
## Function Reference
Complete listing of exported functions with brief descriptions.

### [Category 1]
- `function1(arg1, arg2)`: Detailed description
- `function2(arg1, arg2)`: Detailed description

### [Category 2]
- `function3(arg1, arg2)`: Detailed description
- `function4(arg1, arg2)`: Detailed description

[Additional categories as needed]
{{/if}}

## Alternatives
- Other packages that serve similar purposes
- When to use this package vs. alternatives
- Unique strengths of this package
```

Please ensure your response is:
1. Clear and practical - focus on how users can apply this package to real problems
2. Educational - include enough explanation to understand the examples
3. Accurate - only suggest functionality that actually exists in the package
4. Well-structured - use appropriate headings and organization

For the detail level:
- "brief": Focus on the most essential functions and examples, keep explanations minimal
- "standard": Include main function categories and moderate explanation detail
- "comprehensive": Cover all exported functions, provide detailed explanations and advanced usage

For different focus options:
- "overview": Broad introduction to the package with key functions highlighted
- "functions": Detailed exploration of functions, their purposes and relationships
- "datasets": Focus on included datasets, their structure and usage
- "vignettes": Summarize available vignettes and what users can learn from them
- "dependencies": Analyze package dependencies and how they work together

For function categorization:
- If YES, group functions by purpose/task (data import, visualization, modeling, etc.)
- If NO, present functions in a flat list format

If a package has a large number of functions, prioritize the most commonly used and important ones, especially at the "brief" and "standard" detail levels.
')
}

#' Get template for missing package analysis
#'
#' @return Character string containing the prompt template for missing packages
#' @keywords internal
get_missing_package_analysis_template <- function() {
  return('
You are tldrAI, a tool that provides concise, practical help for R packages.
The user has requested an analysis of the package "{{PACKAGE_NAME}}".

IMPORTANT: The package "{{PACKAGE_NAME}}" is not installed on the user\'s system.

Please provide a helpful response that:
1. Informs the user that the package is not installed
2. Gives instructions on how to install the package
3. Provides information about what the package likely does based on its name and reputation in the R community (if known)
4. Suggests installation with the following command: {{INSTALL_COMMAND}}

Format your response as follows:

```
# Package Not Installed: {{PACKAGE_NAME}}

The package "{{PACKAGE_NAME}}" is not currently installed on your system.

## Installation

You can install the package with:

```r
{{INSTALL_COMMAND}}
```

## About {{PACKAGE_NAME}}

[Brief description of what this package does, based on its name and reputation in the R community]

## Likely Features

[List of likely features or capabilities of this package]

## Common Use Cases

[Description of typical scenarios where this package would be useful]

## Related Packages

[2-3 related packages that might also be useful or serve similar purposes]
```

If the package name is well-known (like dplyr, ggplot2, tidyr, etc.), provide more specific information about its functionality and importance in the R ecosystem. If it\'s not a well-known package, be more cautious in your descriptions and indicate that your information is based on the package name rather than direct knowledge.

Ensure your response is helpful, educational, and encouraging, focusing on helping the user install the package and understand its purpose.
')
}

#' Print formatted package analysis response
#'
#' @param response The raw response from the AI
#' @param package_name Name of the analyzed package
#' @param focus The analysis focus that was used
#' @param categories Whether function categorization was performed
#' @param examples Number of examples requested
#' @param provider The LLM provider that generated the response
#' @param voice The character voice used (if any)
#' @param output Output format ("console", "markdown", "html")
#' @param interactive Whether to create an interactive explorer
#'
#' @return Invisibly returns NULL
#' @keywords internal
print_package_response <- function(response, package_name, focus, categories,
                                examples, provider = NULL, voice = NULL,
                                output = "console", interactive = FALSE) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Printing package response")
    message("DEBUG: Raw response length: ", nchar(response))
  }
  
  # Extract the code block content
  content <- extract_code_block(response)
  
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Extracted content length: ", nchar(content))
  }
  
  # Handle different output formats
  if (output == "markdown") {
    # Return raw markdown
    cat(content)
    return(invisible(NULL))
  } else if (output == "html") {
    # Convert to HTML using rmarkdown
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop("HTML output requires the 'rmarkdown' package")
    }
    
    # Create a temporary markdown file
    temp_md <- tempfile(fileext = ".md")
    writeLines(content, temp_md)
    
    # Convert to HTML
    temp_html <- tempfile(fileext = ".html")
    rmarkdown::render(temp_md, output_file = temp_html, quiet = TRUE)
    
    # Display in viewer if possible, otherwise just open in browser
    if (interactive() && exists(".rs.viewer")) {
      # RStudio environment
      .rs.viewer(temp_html)
    } else {
      utils::browseURL(temp_html)
    }
    
    return(invisible(NULL))
  }
  
  # Default console output
  cat("\n")
  cli::cli_h1(paste0("tldrAI: ", package_name, " Package"))
  
  if (interactive) {
    # Show interactive explorer
    display_interactive_explorer(content, package_name, focus)
  } else {
    # Format the content for console display
    formatted_content <- format_package_content(content)
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Formatted content length: ", nchar(formatted_content))
    }
    cat(formatted_content)
  }
  
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
  
  cat("\n")
  
  invisible(NULL)
}

#' Format package content with markdown-aware rendering
#'
#' @param content The content to format
#'
#' @return Formatted content string
#' @keywords internal
format_package_content <- function(content) {
  # Process markdown headings
  content <- gsub("^# (.+)$", function(m) cli::cli_h1(sub("^# ", "", m)), content, perl = TRUE)
  content <- gsub("^## (.+)$", function(m) cli::cli_h2(sub("^## ", "", m)), content, perl = TRUE)
  content <- gsub("^### (.+)$", function(m) cli::cli_h3(sub("^### ", "", m)), content, perl = TRUE)
  
  # Process code blocks
  content <- gsub("```r\\s*(.+?)\\s*```", function(m) {
    code <- sub("```r\\s*", "", m)
    code <- sub("\\s*```$", "", code)
    paste0("\n", cli::col_silver(code), "\n")
  }, content, perl = TRUE)
  
  # Process bullet points
  content <- gsub("^- (.+)$", function(m) paste0("• ", sub("^- ", "", m)), content, perl = TRUE)
  
  # Return the formatted content
  content
}

#' Display interactive explorer for package analysis
#'
#' @param content The raw content to make interactive
#' @param package_name Name of the analyzed package
#' @param focus The analysis focus that was used
#'
#' @return Invisibly returns NULL
#' @keywords internal
display_interactive_explorer <- function(content, package_name, focus) {
  # Basic interactive menu system for console
  cat("\n")
  cli::cli_h1(paste0("Interactive Explorer: ", package_name, " Package"))
  
  # Parse sections from content
  sections <- parse_sections(content)
  
  # Create a selection menu
  section_names <- names(sections)
  
  if (length(section_names) == 0) {
    cli::cli_alert_warning("No sections found to explore.")
    cat(content)
    return(invisible(NULL))
  }
  
  selected <- 1
  quit <- FALSE
  
  while (!quit) {
    # Clear console (works in most terminals)
    cat("\014")  # Form feed - should clear screen in most terminals
    
    # Show menu
    cli::cli_h1(paste0("Interactive Explorer: ", package_name, " Package"))
    cli::cli_h2("Sections")
    
    for (i in seq_along(section_names)) {
      prefix <- if (i == selected) "→ " else "  "
      cli::cli_text(paste0(prefix, i, ": ", section_names[i]))
    }
    
    cli::cli_text("\nUse up/down arrows or numbers to navigate, Enter to view, q to quit")
    
    # Get user input
    user_input <- readline("Selection: ")
    
    if (tolower(user_input) == "q") {
      quit <- TRUE
    } else if (user_input == "") {
      # Show selected section
      cat("\014")
      cli::cli_h2(section_names[selected])
      cat(sections[[selected]])
      cat("\n\nPress Enter to return to menu...")
      readline()
    } else if (is.numeric(as.numeric(user_input)) && 
              !is.na(as.numeric(user_input)) && 
              as.numeric(user_input) >= 1 && 
              as.numeric(user_input) <= length(section_names)) {
      # Direct selection by number
      selected <- as.numeric(user_input)
      
      # Show selected section
      cat("\014")
      cli::cli_h2(section_names[selected])
      cat(sections[[selected]])
      cat("\n\nPress Enter to return to menu...")
      readline()
    } else if (user_input == "A" || user_input == "a") {
      # Previous section
      selected <- max(1, selected - 1)
    } else if (user_input == "B" || user_input == "b") {
      # Next section
      selected <- min(length(section_names), selected + 1)
    }
  }
  
  invisible(NULL)
}

#' Parse sections from content
#'
#' @param content Content to parse into sections
#'
#' @return Named list of sections
#' @keywords internal
parse_sections <- function(content) {
  # Split by level 2 headings (##)
  matches <- gregexpr("^## .+$", content, perl = TRUE)
  
  if (length(matches[[1]]) <= 1 || matches[[1]][1] == -1) {
    # Not enough sections or no sections found
    return(list("Full Content" = content))
  }
  
  # Extract section positions
  section_positions <- matches[[1]]
  section_lengths <- attr(matches[[1]], "match.length")
  
  # Get section titles
  section_titles <- character(length(section_positions))
  for (i in seq_along(section_positions)) {
    pos <- section_positions[i]
    len <- section_lengths[i]
    title <- substr(content, pos, pos + len - 1)
    title <- gsub("^## ", "", title)
    section_titles[i] <- title
  }
  
  # Extract section contents
  sections <- list()
  for (i in seq_along(section_positions)) {
    start_pos <- section_positions[i] + section_lengths[i]
    end_pos <- if (i < length(section_positions)) {
      section_positions[i + 1] - 1
    } else {
      nchar(content)
    }
    
    section_content <- substr(content, start_pos, end_pos)
    sections[[section_titles[i]]] <- trimws(section_content)
  }
  
  return(sections)
}

#' Analyze a package to extract metadata
#'
#' @param package_name Name of the package to analyze
#' @param package_installed Whether the package is installed
#'
#' @return List of package metadata
#' @keywords internal
analyze_package <- function(package_name, package_installed) {
  # Initialize result structure
  result <- list(
    package = package_name,
    installed = package_installed,
    description = NULL,
    version = NULL,
    exports = character(0),
    datasets = data.frame(),
    vignettes = data.frame(),
    dependencies = character(0),
    imports = character(0),
    suggests = character(0),
    function_docs = list()
  )
  
  if (!package_installed) {
    # If package is not installed, we can't get detailed info
    # Try to get basic info from available packages
    available_pkgs <- as.data.frame(utils::available.packages())
    if (package_name %in% available_pkgs$Package) {
      pkg_idx <- which(available_pkgs$Package == package_name)
      result$description <- available_pkgs$Title[pkg_idx]
      result$version <- available_pkgs$Version[pkg_idx]
    }
    
    return(result)
  }
  
  # Get package description
  pkg_desc <- utils::packageDescription(package_name)
  if (!is.null(pkg_desc)) {
    result$description <- pkg_desc$Title
    result$version <- pkg_desc$Version
    
    # Extract dependencies
    if (!is.null(pkg_desc$Depends)) {
      deps <- strsplit(pkg_desc$Depends, ",")[[1]]
      deps <- trimws(deps)
      deps <- deps[!grepl("^R \\(", deps)]  # Remove R version dependency
      result$dependencies <- deps
    }
    
    # Extract imports
    if (!is.null(pkg_desc$Imports)) {
      imports <- strsplit(pkg_desc$Imports, ",")[[1]]
      imports <- trimws(imports)
      result$imports <- imports
    }
    
    # Extract suggests
    if (!is.null(pkg_desc$Suggests)) {
      suggests <- strsplit(pkg_desc$Suggests, ",")[[1]]
      suggests <- trimws(suggests)
      result$suggests <- suggests
    }
  }
  
  # Get exported functions
  tryCatch({
    ns <- getNamespace(package_name)
    exports <- getNamespaceExports(ns)
    result$exports <- exports
  }, error = function(e) {
    # Just continue if we can't get exports
  })
  
  # Get datasets
  tryCatch({
    datasets <- data(package = package_name)$results
    if (nrow(datasets) > 0) {
      result$datasets <- datasets
    }
  }, error = function(e) {
    # Just continue if we can't get datasets
  })
  
  # Get vignettes
  tryCatch({
    vignettes <- vignette(package = package_name)$results
    if (!is.null(vignettes) && nrow(vignettes) > 0) {
      result$vignettes <- vignettes
    }
  }, error = function(e) {
    # Just continue if we can't get vignettes
  })
  
  # Get function documentation for exported functions
  if (length(result$exports) > 0) {
    # Limit to a reasonable number to avoid making the prompt too large
    max_docs <- 50  # Adjust based on testing
    exports_subset <- result$exports
    if (length(exports_subset) > max_docs) {
      # Try to select important functions first (if possible)
      # This is a heuristic approach - we assume functions with shorter names 
      # or those that have the package name in them might be more central
      
      # Score each function (lower is better)
      scores <- sapply(exports_subset, function(f) {
        score <- nchar(f)  # Shorter names get lower scores
        
        # If the function has the package name in it, lower the score
        if (grepl(package_name, f, ignore.case = TRUE)) {
          score <- score - 5
        }
        
        # Common important function patterns get lower scores
        if (grepl("^plot|^print|^summary|^predict|^fit|^main|^run", f)) {
          score <- score - 3
        }
        
        return(score)
      })
      
      # Sort by score and take the top max_docs
      exports_subset <- exports_subset[order(scores)][1:max_docs]
    }
    
    # Get documentation for each function
    for (func in exports_subset) {
      tryCatch({
        help_text <- utils::capture.output(utils::help(func, package = package_name))
        if (length(help_text) > 0) {
          # Process help text to make it more readable
          help_text <- paste(help_text, collapse = "\n")
          
          # Add to function docs
          result$function_docs <- c(result$function_docs, help_text)
        }
      }, error = function(e) {
        # Just continue if we can't get help for a specific function
      })
    }
  }
  
  return(result)
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}