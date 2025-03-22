#' Get metadata about an R function
#'
#' @param func_name The name of the function
#' @param package The package name (optional)
#'
#' @return A list with function metadata
#' @keywords internal
get_function_metadata <- function(func_name, package = NULL) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Getting metadata for function: ", func_name)
    if (!is.null(package)) {
      message("DEBUG: Package specified: ", package)
    }
  }
  
  # Try to get the function from specified package first
  if (!is.null(package)) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is not installed or cannot be loaded")
    }
    
    # Check if function exists in the package
    if (exists(func_name, envir = asNamespace(package), inherits = FALSE)) {
      func <- get(func_name, envir = asNamespace(package))
      
      # Get basic function information
      args <- names(formals(func))
      args_str <- paste(args, collapse = ", ")
      
      # Get function body summary
      body_str <- deparse(body(func))
      body_str <- paste(body_str[1:min(5, length(body_str))], collapse = " ")
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Found function in specified package")
        message("DEBUG: Arguments: ", args_str)
      }
      
      return(list(
        name = func_name,
        package = package,
        signature = get_function_signature(func_name, func),
        description = get_function_description(func_name, package),
        args = args,
        body_summary = body_str
      ))
    } else {
      stop("Function '", func_name, "' not found in package '", package, "'")
    }
  }
  
  # Get the function from global environment
  if (exists(func_name, mode = "function")) {
    func <- get(func_name, mode = "function")
    pkg <- find_package(func_name)
    
    # Get basic function information
    args <- names(formals(func))
    args_str <- paste(args, collapse = ", ")
    
    # Get function body summary
    body_str <- deparse(body(func))
    body_str <- paste(body_str[1:min(5, length(body_str))], collapse = " ")
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Found function in global environment")
      message("DEBUG: Package detected: ", pkg)
      message("DEBUG: Arguments: ", args_str)
    }
    
    return(list(
      name = func_name,
      package = pkg,
      signature = get_function_signature(func_name, func),
      description = get_function_description(func_name, pkg),
      args = args,
      body_summary = body_str
    ))
  } else {
    # Try to find the function in loaded packages
    matches <- utils::apropos(func_name, mode = "function")
    
    if (length(matches) == 0) {
      stop("Function '", func_name, "' not found")
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Found ", length(matches), " potential matches: ", paste(matches, collapse = ", "))
    }
    
    # If there are multiple matches, use the exact match if available
    if (length(matches) > 1) {
      exact_match <- matches[matches == func_name]
      if (length(exact_match) == 1) {
        func_name <- exact_match
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Using exact match: ", func_name)
        }
      } else {
        # Find the best match, prioritizing those from common packages
        common_packages <- c("base", "stats", "utils", "graphics", "ggplot2", "dplyr", "tidyr", "stringr", "lubridate", "purrr")
        for (pkg in common_packages) {
          pkg_namespace <- tryCatch(asNamespace(pkg), error = function(e) NULL)
          if (!is.null(pkg_namespace)) {
            for (match in matches) {
              if (exists(match, envir = pkg_namespace, inherits = FALSE)) {
                func_name <- match
                
                if (get_config("debug_mode", default = FALSE)) {
                  message("DEBUG: Using match from common package: ", func_name, " from ", pkg)
                }
                
                warning("Function ambiguity detected: multiple functions named '", func_name, "' found. Using the one from '", pkg, "' package.\n\n",
                        "To get the correct help:\n",
                        "  * tldr('", pkg, "::", func_name, "')\n",
                        "  * Or first load the package with library(", pkg, ")")
                
                func <- get(func_name, envir = pkg_namespace)
                
                # Get basic function information
                args <- names(formals(func))
                args_str <- paste(args, collapse = ", ")
                
                # Get function body summary
                body_str <- deparse(body(func))
                body_str <- paste(body_str[1:min(5, length(body_str))], collapse = " ")
                
                return(list(
                  name = func_name,
                  package = pkg,
                  signature = get_function_signature(func_name, func),
                  description = get_function_description(func_name, pkg),
                  args = args,
                  body_summary = body_str
                ))
              }
            }
          }
        }
        
        # If no match in common packages, use the first one
        func_name <- matches[1]
        pkg_guess <- find_package(func_name)
        
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: No match in common packages, using first match: ", func_name)
        }
        
        # Check if the function might be from ggplot2, dplyr, or other popular packages
        suggested_packages <- c()
        for (potential_pkg in c("ggplot2", "dplyr", "tidyr", "purrr", "stringr", "lubridate")) {
          if (requireNamespace(potential_pkg, quietly = TRUE)) {
            if (exists(func_name, envir = asNamespace(potential_pkg), inherits = FALSE)) {
              suggested_packages <- c(suggested_packages, potential_pkg)
            }
          }
        }
        
        if (length(suggested_packages) > 0) {
          suggestions <- paste0(suggested_packages, "::", func_name)
          suggestion_text <- paste(suggestions, collapse = "' or '")
          warning("Function ambiguity detected: multiple functions named '", func_name, "' found. Using '", pkg_guess, "::", func_name, "'.\n\n",
                  "To get help for a specific package's function, try:\n",
                  "  * tldr('", suggestion_text, "')\n",
                  "  * First load the package with library(", suggested_packages[1], ")\n\n",
                  "Popular packages with a '", func_name, "' function: ", paste(suggested_packages, collapse = ", "))
        } else {
          warning("Multiple functions named '", func_name, "' found. Using '", pkg_guess, "::", func_name, "'.\n",
                  "For more precise results, use the package::function notation (e.g., 'packagename::", func_name, "')")
      }
    } else {
      func_name <- matches[1]
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Using single match: ", func_name)
      }
    }
    
    func <- get(func_name, mode = "function")
    pkg <- find_package(func_name)
    
    # Get basic function information
    args <- names(formals(func))
    args_str <- paste(args, collapse = ", ")
    
    # Get function body summary
    body_str <- deparse(body(func))
    body_str <- paste(body_str[1:min(5, length(body_str))], collapse = " ")
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Found function in package: ", pkg)
      message("DEBUG: Arguments: ", args_str)
    }
    
    return(list(
      name = func_name,
      package = pkg,
      signature = get_function_signature(func_name, func),
      description = get_function_description(func_name, pkg),
      args = args,
      body_summary = body_str
    ))
  }
}

#' Find the package that a function belongs to
#'
#' @param func_name The name of the function
#'
#' @return Character string with the package name
#' @keywords internal
find_package <- function(func_name) {
  # Look for the function in all loaded namespaces
  for (pkg in loadedNamespaces()) {
    if (exists(func_name, envir = asNamespace(pkg), inherits = FALSE)) {
      return(pkg)
    }
  }
  
  # Check if it's in base R
  if (exists(func_name, envir = baseenv(), inherits = FALSE)) {
    return("base")
  }
  
  # Default to unknown
  "unknown"
}

#' Get the function signature as a string
#'
#' @param func_name The name of the function
#' @param func The function object
#'
#' @return Character string with the formatted signature
#' @keywords internal
get_function_signature <- function(func_name, func) {
  # Get formals (arguments)
  args <- formals(func)
  
  # Format each argument
  arg_strings <- character(length(args))
  for (i in seq_along(args)) {
    arg_name <- names(args)[i]
    if (i <= length(args)) {
      # Get the argument value as a string safely
      arg_value_str <- tryCatch({
        arg_value <- args[[i]]
        if (is.symbol(arg_value)) {
          if (as.character(arg_value) == "") {
            # Just the argument name for missing default values
            ""
          } else {
            # Symbol with a value
            paste0(" = ", as.character(arg_value))
          }
        } else {
          # Non-symbol value
          paste0(" = ", deparse(arg_value, width.cutoff = 500)[1])
        }
      }, error = function(e) {
        "" # Return empty string on error
      })
      
      # Combine name and value
      arg_strings[i] <- paste0(arg_name, arg_value_str)
    } else {
      # Fallback for unexpected cases
      arg_strings[i] <- arg_name
    }
  }
  
  # Build the signature
  paste0(func_name, "(", paste(arg_strings, collapse = ", "), ")")
}

#' Show details about a function's metadata
#'
#' @param func_name The name of the function
#' @param package The package name (optional)
#'
#' @return Invisibly returns the function metadata
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_function_details("mean")
#' tldr_function_details("filter", "dplyr")
#' }
tldr_function_details <- function(func_name, package = NULL) {
  # Enable debug mode temporarily
  old_debug <- get_config("debug_mode", default = FALSE)
  tryCatch({
    tldr_config(debug_mode = TRUE)
    
    # Get function metadata
    metadata <- get_function_metadata(func_name, package)
    
    # Print metadata
    cat("\nFunction Details:", func_name, "\n")
    cat("=======================\n")
    cat("Package:", metadata$package, "\n")
    cat("Signature:", metadata$signature, "\n")
    cat("Description:", metadata$description, "\n")
    cat("Arguments:", paste(metadata$args, collapse = ", "), "\n")
    cat("Body (excerpt):", metadata$body_summary, "\n")
    cat("\n")
    
    # Get the prompt that would be generated
    prompt <- build_prompt(func_name, metadata, verbose = TRUE, examples = 2)
    cat("Prompt for LLM:\n")
    cat("=======================\n")
    cat(prompt, "\n")
    
    invisible(metadata)
  }, finally = {
    # Restore original debug setting
    tldr_config(debug_mode = old_debug)
  })
}

#' Get the function description from help pages
#'
#' @param func_name The name of the function
#' @param package The package name
#'
#' @return Character string with the function description
#' @keywords internal
get_function_description <- function(func_name, package) {
  # Try to get help for the function
  help_path <- try(utils::help(func_name, package = package), silent = TRUE)
  
  if (inherits(help_path, "try-error") || length(help_path) == 0) {
    return("No description available")
  }
  
  # Extract the description from the help page
  help_text <- utils::capture.output(utils::help(func_name, package = package))
  
  # Parse the help text to extract the description
  start_idx <- which(grepl("^Description:", help_text))
  end_idx <- which(grepl("^Usage:|^Arguments:|^Value:", help_text))
  end_idx <- end_idx[end_idx > start_idx][1]
  
  if (length(start_idx) == 0 || length(end_idx) == 0) {
    return("No description available")
  }
  
  description <- paste(help_text[(start_idx + 1):(end_idx - 1)], collapse = " ")
  description <- gsub("^ +", "", description)  # Remove leading spaces
  
  if (description == "") {
    return("No description available")
  }
  
  description
}