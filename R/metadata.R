#' Get metadata about an R function
#'
#' @param func_name The name of the function
#' @param package The package name (optional)
#'
#' @return A list with function metadata
#' @keywords internal
get_function_metadata <- function(func_name, package = NULL) {
  # Try to get the function from specified package first
  if (!is.null(package)) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is not installed or cannot be loaded")
    }
    
    # Check if function exists in the package
    if (exists(func_name, envir = asNamespace(package), inherits = FALSE)) {
      func <- get(func_name, envir = asNamespace(package))
      return(list(
        name = func_name,
        package = package,
        signature = get_function_signature(func_name, func),
        description = get_function_description(func_name, package)
      ))
    } else {
      stop("Function '", func_name, "' not found in package '", package, "'")
    }
  }
  
  # Get the function from global environment
  if (exists(func_name, mode = "function")) {
    func <- get(func_name, mode = "function")
    pkg <- find_package(func_name)
    
    return(list(
      name = func_name,
      package = pkg,
      signature = get_function_signature(func_name, func),
      description = get_function_description(func_name, pkg)
    ))
  } else {
    # Try to find the function in loaded packages
    matches <- utils::apropos(func_name, mode = "function")
    
    if (length(matches) == 0) {
      stop("Function '", func_name, "' not found")
    }
    
    # If there are multiple matches, use the exact match if available
    if (length(matches) > 1) {
      exact_match <- matches[matches == func_name]
      if (length(exact_match) == 1) {
        func_name <- exact_match
      } else {
        # Find the best match, prioritizing those from common packages
        common_packages <- c("base", "stats", "utils", "graphics", "ggplot2", "dplyr", "tidyr", "stringr", "lubridate", "purrr")
        for (pkg in common_packages) {
          pkg_namespace <- tryCatch(asNamespace(pkg), error = function(e) NULL)
          if (!is.null(pkg_namespace)) {
            for (match in matches) {
              if (exists(match, envir = pkg_namespace, inherits = FALSE)) {
                func_name <- match
                warning("Multiple functions named '", func_name, "' found. Using the one from '", pkg, "' package.\n",
                        "For a specific package, use the package::function notation (e.g., '", pkg, "::", func_name, "')")
                return(list(
                  name = func_name,
                  package = pkg,
                  signature = get_function_signature(func_name, get(func_name, envir = pkg_namespace)),
                  description = get_function_description(func_name, pkg)
                ))
              }
            }
          }
        }
        
        # If no match in common packages, use the first one
        func_name <- matches[1]
        warning("Multiple functions named '", func_name, "' found. Using '", func_name, "'.\n",
                "For more precise results, use the package::function notation (e.g., 'packagename::", func_name, "')")
      }
    } else {
      func_name <- matches[1]
    }
    
    func <- get(func_name, mode = "function")
    pkg <- find_package(func_name)
    
    return(list(
      name = func_name,
      package = pkg,
      signature = get_function_signature(func_name, func),
      description = get_function_description(func_name, pkg)
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