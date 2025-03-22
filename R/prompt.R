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
  template <- get_prompt_template()
  
  # Replace placeholders in template
  prompt <- template
  prompt <- gsub("{{FUNCTION_NAME}}", func_name, prompt)
  prompt <- gsub("{{FUNCTION_SIGNATURE}}", func_metadata$signature, prompt)
  prompt <- gsub("{{FUNCTION_DESCRIPTION}}", func_metadata$description, prompt)
  prompt <- gsub("{{PACKAGE_NAME}}", func_metadata$package, prompt)
  prompt <- gsub("{{EXAMPLES_REQUESTED}}", as.character(examples), prompt)
  prompt <- gsub("{{VERBOSE}}", ifelse(verbose, "YES", "NO"), prompt)
  
  prompt
}

#' Get the default prompt template
#'
#' @return Character string containing the prompt template
#' @keywords internal
get_prompt_template <- function() {
  # This could be customizable and loaded from a file in the future
  return('
You are tldrAI, a tool that provides concise, practical help for R functions.
Your goal is to create a brief, helpful summary of the R function {{FUNCTION_NAME}} from the {{PACKAGE_NAME}} package.

Here is information about the function:
Function signature: {{FUNCTION_SIGNATURE}}
Function description: {{FUNCTION_DESCRIPTION}}

Please provide a response in the following format:

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