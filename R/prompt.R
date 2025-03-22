#' Build the prompt for the AI
#'
#' @param func_name The name of the function
#' @param func_metadata Metadata about the function
#' @param verbose Whether to request verbose output
#' @param examples Number of examples to request
#' @param context_data Optional context data from the context analyzer
#'
#' @return Character string containing the prompt
#' @keywords internal
build_prompt <- function(func_name, func_metadata, verbose, examples, context_data = NULL) {
  # Check for missing package or function
  if (!is.null(func_metadata$missing_package) && func_metadata$missing_package) {
    # Use a special template for missing packages
    template <- get_missing_package_template()
    template <- gsub("{{FUNCTION_NAME}}", func_name, template, fixed = TRUE)
    template <- gsub("{{PACKAGE_NAME}}", func_metadata$package, template, fixed = TRUE)
    template <- gsub("{{INSTALL_COMMAND}}", func_metadata$install_command, template, fixed = TRUE)
    return(template)
  } else if (!is.null(func_metadata$missing_function) && func_metadata$missing_function) {
    # Use a special template for missing functions
    template <- get_missing_function_template()
    template <- gsub("{{FUNCTION_NAME}}", func_name, template, fixed = TRUE)
    template <- gsub("{{PACKAGE_NAME}}", func_metadata$package, template, fixed = TRUE)
    return(template)
  }
  
  # For normal functions, proceed as before
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
  
  # Add context data if available
  if (!is.null(context_data)) {
    context_str <- context_data
    template <- gsub("{{CONTEXT_DATA}}", context_str, template, fixed = TRUE)
    template <- gsub("{{HAS_CONTEXT}}", "YES", template, fixed = TRUE)
  } else {
    template <- gsub("{{CONTEXT_DATA}}", "", template, fixed = TRUE)
    template <- gsub("{{HAS_CONTEXT}}", "NO", template, fixed = TRUE)
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

Context awareness mode is set to: {{HAS_CONTEXT}}
{{CONTEXT_DATA}}

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

If context awareness mode is YES:
1. Make examples relevant to the user\'s environment, using their actual data frames and variables when appropriate
2. Tailor examples to fit with packages they already have loaded
3. Show how this function fits into their current workflow based on command history
4. Consider the user\'s data structures when explaining function use cases
5. If the function works with data frames, use column names from the user\'s actual data frames in examples

Keep all responses concise and focused on practical usage. Prioritize examples over lengthy explanations.
')
}

#' Get template for missing package
#'
#' @return Character string containing the prompt template for missing packages
#' @keywords internal
get_missing_package_template <- function() {
  return('
You are tldrAI, a tool that provides concise, practical help for R functions.
The user has requested help with the function "{{FUNCTION_NAME}}" from the package "{{PACKAGE_NAME}}".

IMPORTANT: The package "{{PACKAGE_NAME}}" is not installed on the user\'s system.

Please provide a helpful response that:
1. Informs the user that the package is not installed
2. Gives instructions on how to install the package
3. Provides a brief general description of what the function likely does (based on its name and the package)
4. Suggests installation with the following command: {{INSTALL_COMMAND}}

Format your response as follows:

```
# Package Not Installed: {{PACKAGE_NAME}}

The package "{{PACKAGE_NAME}}" is not currently installed on your system. This package is required to use the function "{{FUNCTION_NAME}}".

## Installation

You can install the package with:

```r
{{INSTALL_COMMAND}}
```

## About {{FUNCTION_NAME}}

Based on the function name, here\'s what I expect this function does. After installing the package, use `tldr("{{FUNCTION_NAME}}")` or `?{{FUNCTION_NAME}}` to get accurate documentation.

[Brief description of what the function likely does, based on its name and typical functions in this package]

## Related Functions

[2-3 related functions that might also be useful from this package]
```

Ensure your response is helpful, educational, and encouraging, focusing on helping the user install the package and understand its purpose.
')
}

#' Get template for missing function
#'
#' @return Character string containing the prompt template for missing functions
#' @keywords internal
get_missing_function_template <- function() {
  return('
You are tldrAI, a tool that provides concise, practical help for R functions.
The user has requested help with the function "{{FUNCTION_NAME}}" from the package "{{PACKAGE_NAME}}".

IMPORTANT: The function "{{FUNCTION_NAME}}" could not be found in the installed package "{{PACKAGE_NAME}}".

Please provide a helpful response that:
1. Informs the user that the function could not be found in the specified package
2. Suggests possible reasons (typo, function removed in newer versions, function in different package, etc.)
3. Suggests checking the package documentation with `help(package="{{PACKAGE_NAME}}")` to see available functions
4. If the function name suggests what it might do, provide suggestions for alternative functions

Format your response as follows:

```
# Function Not Found: {{FUNCTION_NAME}}

The function "{{FUNCTION_NAME}}" could not be found in the installed package "{{PACKAGE_NAME}}".

## Possible Reasons

- The function name might contain a typo
- The function may have been renamed or removed in your version of the package
- The function might be in a different package with a similar name

## Check Available Functions

To see all functions available in the {{PACKAGE_NAME}} package:

```r
help(package="{{PACKAGE_NAME}}")
```

## Possible Alternatives

[List 2-3 potential alternative functions that might provide similar functionality, based on the function name]
```

Ensure your response is helpful and educational, focusing on helping the user find the right function.
')
}