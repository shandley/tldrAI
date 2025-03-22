#' Build the prompt for the AI
#'
#' @param func_name The name of the function
#' @param func_metadata Metadata about the function
#' @param verbose Whether to request verbose output
#' @param examples Number of examples to request
#'
#' @return Character string containing the prompt
#' @keywords internal
build_prompt <- function(func_name, func_metadata, verbose, examples) {
  # Get the template
  template <- get_prompt_template()
  
  # Replace placeholders with actual values
  template <- gsub("{{FUNCTION_NAME}}", func_name, template, fixed = TRUE)
  template <- gsub("{{PACKAGE_NAME}}", func_metadata$package, template, fixed = TRUE)
  template <- gsub("{{FUNCTION_SIGNATURE}}", func_metadata$signature, template, fixed = TRUE)
  
  # Handle description with fallback
  description_str <- "No description available"
  if (!is.null(func_metadata$description) && func_metadata$description != "No description available") {
    description_str <- func_metadata$description
  }
  template <- gsub("{{FUNCTION_DESCRIPTION}}", description_str, template, fixed = TRUE)
  
  # Handle argument list
  args_str <- "None specified"
  if (!is.null(func_metadata$args) && length(func_metadata$args) > 0) {
    args_str <- paste(func_metadata$args, collapse = ", ")
  }
  template <- gsub("{{FUNCTION_ARGS}}", args_str, template, fixed = TRUE)
  
  # Handle function body/implementation (if available)
  body_str <- "Not available"
  if (!is.null(func_metadata$body)) {
    body_str <- func_metadata$body
  } else if (!is.null(func_metadata$body_summary)) {
    body_str <- func_metadata$body_summary
  }
  template <- gsub("{{FUNCTION_BODY}}", body_str, template, fixed = TRUE)
  
  # Handle examples count
  template <- gsub("{{EXAMPLES_REQUESTED}}", as.character(examples), template, fixed = TRUE)
  
  # Handle verbose flag
  verbose_str <- ifelse(verbose, "YES", "NO")
  template <- gsub("{{VERBOSE}}", verbose_str, template, fixed = TRUE)
  
  template
}

#' Get the default prompt template
#'
#' @return Character string containing the prompt template
#' @keywords internal
get_prompt_template <- function() {
  # This could be customizable and loaded from a file in the future
  return('
You are tldrAI, a tool that provides concise, practical help for R functions.
Your goal is to create a brief, helpful summary of the R function "{{FUNCTION_NAME}}" (exactly as written) from the {{PACKAGE_NAME}} package.

IMPORTANT: You MUST only provide information about the exact function named "{{FUNCTION_NAME}}" and nothing else. Do not provide information about other R functions with similar names or from different packages.

Here is detailed information about the function:
Function name: "{{FUNCTION_NAME}}" (You MUST document exactly this function)
Function signature: {{FUNCTION_SIGNATURE}}
Function description: {{FUNCTION_DESCRIPTION}}
Function arguments: {{FUNCTION_ARGS}}
Function implementation: {{FUNCTION_BODY}}

Please provide a response in the following format, and ONLY for the function "{{FUNCTION_NAME}}" - you MUST NOT document any other function:

```
# {{FUNCTION_NAME}}

## Purpose
A single sentence explaining what this function does, focusing on its practical use.

## Usage
The simplest way to use this function, showing only the most essential arguments.

## Key Arguments
- arg1: Brief explanation
- arg2: Brief explanation
(limit to the 3-5 most important arguments)

## Examples
```r
# Brief comment explaining the example
{{FUNCTION_NAME}}(...) # Simple usage
```

(Provide {{EXAMPLES_REQUESTED}} practical, realistic examples that show common use cases)
```

Verbose mode is set to: {{VERBOSE}}
If verbose is YES, include additional sections on:
1. Common gotchas or pitfalls
2. Related functions that are commonly used with this one
3. A brief note on when to use this function vs alternatives

Keep all responses concise and focused on practical usage. Prioritize examples over lengthy explanations.
')
}