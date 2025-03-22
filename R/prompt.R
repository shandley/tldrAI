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
  # Get the raw template first
  template <- "
I need help documenting the R function 'mean' from the base package.

Please write a concise, practical help summary for the 'mean' function that explains:

1. Its purpose (calculating the arithmetic mean of values)
2. Basic usage (mean(x))
3. Key arguments (what x can be, and what the ... is for)
4. 2-3 practical examples of using the function
5. Common pitfalls or gotchas
6. Related functions (like median, sum, etc.)
7. When to use mean vs alternatives

Format the response as a markdown document with clear section headers.
"
  
  # Replace the function name and package with the actual values
  if (func_name != "mean") {
    template <- gsub("mean", func_name, template, fixed = TRUE)
  }
  if (func_metadata$package != "base") {
    template <- gsub("base", func_metadata$package, template, fixed = TRUE)
  }
  
  # Add specific signature information
  if (!is.null(func_metadata$signature)) {
    signature_info <- paste0("\nThe function signature is: ", func_metadata$signature)
    template <- paste0(template, signature_info)
  }
  
  # Add arguments info
  if (!is.null(func_metadata$args)) {
    args_str <- paste(func_metadata$args, collapse = ", ")
    args_info <- paste0("\nThe function arguments are: ", args_str)
    template <- paste0(template, args_info)
  }
  
  # Add examples request
  examples_info <- paste0("\nPlease include ", as.character(examples), " practical examples.")
  template <- paste0(template, examples_info)
  
  # Add verbose flag
  if (verbose) {
    template <- paste0(template, "\nPlease include detailed information about common pitfalls, related functions, and when to use this function vs alternatives.")
  }
  
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