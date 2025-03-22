# Implementation of visualization types

# Helper function to get actual function source code
get_function_source <- function(func_name, pkg, args_str) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Retrieving source code for ", func_name, " in package ", pkg)
  }
  
  # Try to get the actual function source
  func_source <- tryCatch({
    # Handle special cases for base/internal functions
    if (pkg %in% c("base", "utils", "stats", "graphics")) {
      # Check if it's a primitive function
      f <- get(func_name, envir = asNamespace(pkg))
      if (is.primitive(f)) {
        return(paste0("function(", args_str, ") {\n  # This is a primitive function\n  # in package ", pkg, "\n  # Source code not available\n}"))
      }
    }
    
    # Try to get source code
    if (requireNamespace(pkg, quietly = TRUE)) {
      # Get the function from the package
      f <- get(func_name, envir = asNamespace(pkg))
      
      # For S3 methods, follow the actual implementation
      if (inherits(f, "standardGeneric")) {
        return(paste0("function(", args_str, ") {\n  # This is an S4 generic function\n  # in package ", pkg, "\n  # Source code varies by method\n}"))
      }
      
      # Get the source code
      src <- deparse(f)
      if (length(src) > 0) {
        return(paste(src, collapse = "\n"))
      }
    }
    
    return(NULL)
  }, error = function(e) {
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Error retrieving source code: ", e$message)
    }
    return(NULL)
  })
  
  return(func_source)
}

# Helper function to get related functions for a given function/package
get_related_functions <- function(func_name, pkg, max_funcs = 5) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Finding related functions for ", func_name, " in package ", pkg)
  }
  
  # Common packages with predefined relationships
  if (pkg == "dplyr") {
    return(c("select", "mutate", "summarize", "arrange", "group_by"))
  } else if (pkg == "ggplot2") {
    return(c("aes", "geom_point", "geom_line", "theme", "facet_wrap"))
  } else if (pkg == "stats") {
    return(c("t.test", "lm", "glm", "anova", "predict"))
  } else if (pkg == "base") {
    return(c("mean", "sum", "apply", "sapply", "lapply"))
  } else if (pkg == "tidyr") {
    return(c("pivot_longer", "pivot_wider", "nest", "unnest", "separate"))
  } else if (pkg == "stringr") {
    return(c("str_detect", "str_replace", "str_extract", "str_split", "str_trim"))
  } else if (pkg == "purrr") {
    return(c("map", "map_dbl", "map_chr", "reduce", "pluck"))
  }
  
  # For other packages, try to find related functions
  pkg_funcs <- tryCatch({
    # Make sure package is loaded
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(c())
    }
    
    # Get functions in the package namespace
    ns <- asNamespace(pkg)
    funcs <- ls(ns)
    
    # Filter to functions only and exclude hidden functions
    funcs <- funcs[!grepl("^\\.", funcs)]
    funcs <- funcs[sapply(funcs, function(x) is.function(get(x, envir = ns)))]
    
    # Check if current function exists
    if (!(func_name %in% funcs)) {
      return(c())
    }
    
    # If we have function details, try to find similar functions with same prefix
    prefix <- gsub("_.*$", "", func_name)
    similar_prefix <- grep(paste0("^", prefix), funcs, value = TRUE)
    
    # If we found prefix matches, use them
    if (length(similar_prefix) > 1) {
      similar_prefix <- setdiff(similar_prefix, func_name)
      if (length(similar_prefix) > max_funcs) {
        similar_prefix <- similar_prefix[1:max_funcs]
      }
      return(similar_prefix)
    }
    
    # Otherwise, return some random functions from the package
    other_funcs <- setdiff(funcs, func_name)
    if (length(other_funcs) == 0) {
      return(c())
    }
    
    # Randomize and return up to max_funcs
    other_funcs <- sample(other_funcs, min(max_funcs, length(other_funcs)))
    return(other_funcs)
  }, error = function(e) {
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Error finding related functions: ", e$message)
    }
    return(c())
  })
  
  return(pkg_funcs)
}

# Basic diagram visualization
generate_diagram <- function(func_name, metadata) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generating DiagrammeR diagram for ", func_name)
  }
  
  # Extract metadata
  args <- metadata$args %||% character(0)
  package <- metadata$package %||% "unknown"
  description <- metadata$description %||% "No description available"
  
  # Create a simple diagram using DiagrammeR with DOT language
  dot_code <- paste0(
    "digraph G {\n",
    "  # Graph options\n",
    "  graph [fontname = \"Arial\", rankdir = TB]\n",
    "  node [fontname = \"Arial\", shape = box, style = filled]\n",
    "  edge [fontname = \"Arial\"]\n\n",
    
    "  # Function node\n",
    "  func [label = \"", func_name, "()\", fillcolor = \"#D4EFDF\", fontsize = 14, width = 2.5, height = 1, penwidth = 2]\n\n",
    
    "  # Package node\n",
    "  pkg [label = \"Package: ", package, "\", fillcolor = \"#E8F8F5\", fontsize = 12]\n\n",
    
    "  # Function to package relation\n",
    "  func -> pkg [color = \"#2E86C1\"]\n\n",
    
    "  # Argument nodes\n"
  )
  
  # Add each argument as a node
  for (i in seq_along(args)) {
    arg_name <- gsub("\\.", "_", args[i]) # DOT doesn't like dots in node names
    dot_code <- paste0(
      dot_code,
      "  arg", i, " [label = \"", args[i], "\", shape = ellipse, fillcolor = \"#F5EEF8\", fontsize = 10]\n",
      "  func -> arg", i, " [color = \"#884EA0\"]\n"
    )
  }
  
  # Close the graph
  dot_code <- paste0(dot_code, "}\n")
  
  # Create and return the diagram
  diagram <- DiagrammeR::grViz(dot_code)
  return(diagram)
}

# Flowchart visualization
generate_flowchart <- function(func_name, metadata) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generating DiagrammeR flowchart for ", func_name)
  }
  
  # Extract metadata
  args <- metadata$args %||% character(0)
  pkg <- metadata$package %||% "Unknown"
  
  # Determine if function likely uses loops or conditionals
  has_loop <- any(grepl("loop|for|while|each|apply", func_name, ignore.case = TRUE)) || 
              any(grepl("loop|for|while|each|apply", args, ignore.case = TRUE))
  
  has_conditional <- any(grepl("if|condition|test|check", func_name, ignore.case = TRUE)) || 
                     any(grepl("if|condition|test|check", args, ignore.case = TRUE))
  
  # Create DOT language flowchart
  dot_code <- paste0(
    "digraph G {\n",
    "  # Graph options\n",
    "  graph [fontname = \"Arial\", rankdir = TB]\n",
    "  node [fontname = \"Arial\", style = filled]\n",
    "  edge [fontname = \"Arial\"]\n\n",
    
    "  # Start node\n",
    "  start [label = \"Start\", shape = oval, fillcolor = \"#D4EFDF\", penwidth = 2]\n\n",
    
    "  # Function node\n",
    "  func [label = \"", func_name, "()\", shape = box, fillcolor = \"#85C1E9\", fontsize = 14, penwidth = 2]\n\n",
    
    "  # Process node\n",
    "  process [label = \"Process Input Data\", shape = box, fillcolor = \"#E8F8F5\"]\n\n",
    
    "  # Connect main flow\n",
    "  start -> func [color = \"#2C3E50\"]\n",
    "  func -> process [color = \"#2C3E50\"]\n\n"
  )
  
  # Add conditional path if needed
  if (has_conditional) {
    dot_code <- paste0(
      dot_code,
      "  # Conditional path\n",
      "  condition [label = \"Condition?\", shape = diamond, fillcolor = \"#FAD7A0\"]\n",
      "  true_path [label = \"True Path\", shape = box, fillcolor = \"#ABEBC6\"]\n",
      "  false_path [label = \"False Path\", shape = box, fillcolor = \"#F5B7B1\"]\n",
      "  merge [label = \"Merge Paths\", shape = box, fillcolor = \"#E8F8F5\"]\n\n",
      
      "  # Connect conditional paths\n",
      "  process -> condition [color = \"#2C3E50\"]\n",
      "  condition -> true_path [color = \"green4\", label = \"Yes\"]\n",
      "  condition -> false_path [color = \"red3\", label = \"No\"]\n",
      "  true_path -> merge [color = \"#2C3E50\"]\n",
      "  false_path -> merge [color = \"#2C3E50\"]\n\n"
    )
    
    next_node <- "merge"
  } else {
    next_node <- "process"
  }
  
  # Add loop path if needed
  if (has_loop) {
    dot_code <- paste0(
      dot_code,
      "  # Loop path\n",
      "  loop_condition [label = \"More Items?\", shape = diamond, fillcolor = \"#FAD7A0\"]\n",
      "  process_item [label = \"Process Item\", shape = box, fillcolor = \"#E8F8F5\"]\n\n",
      
      "  # Connect loop paths\n",
      "  ", next_node, " -> loop_condition [color = \"#2C3E50\"]\n",
      "  loop_condition -> process_item [color = \"green4\", label = \"Yes\"]\n",
      "  process_item -> loop_condition [color = \"#2C3E50\"]\n\n"
    )
    
    next_node <- "loop_condition"
  }
  
  # Add result and end nodes
  dot_code <- paste0(
    dot_code,
    "  # Result and end nodes\n",
    "  result [label = \"Return Result\", shape = box, fillcolor = \"#D4EFDF\"]\n",
    "  end [label = \"End\", shape = oval, fillcolor = \"#D4EFDF\", penwidth = 2]\n\n",
    
    "  # Connect final flow\n"
  )
  
  if (has_loop) {
    dot_code <- paste0(
      dot_code,
      "  loop_condition -> result [color = \"red3\", label = \"No\"]\n"
    )
  } else {
    dot_code <- paste0(
      dot_code,
      "  ", next_node, " -> result [color = \"#2C3E50\"]\n"
    )
  }
  
  dot_code <- paste0(
    dot_code,
    "  result -> end [color = \"#2C3E50\"]\n",
    "}\n"
  )
  
  # Create and return the diagram
  diagram <- DiagrammeR::grViz(dot_code)
  return(diagram)
}

# Data Flow visualization
generate_data_flow_diagram <- function(func_name, metadata) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generating data flow diagram for ", func_name)
  }
  
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    message("DiagrammeR package not available, using ASCII fallback")
    return(generate_ascii_data_flow(func_name, metadata))
  }
  
  message("Using DiagrammeR for data flow visualization")
  
  # Extract metadata
  args <- metadata$args %||% character(0)
  pkg <- metadata$package %||% "Unknown"
  
  # Determine function type for customization
  is_dplyr <- pkg %in% c("dplyr", "tidyr", "purrr")
  is_ggplot <- pkg %in% c("ggplot2", "ggvis")
  is_stats <- pkg %in% c("stats", "lme4", "caret")
  
  # Start building DOT code for the graph
  dot_code <- paste0(
    "digraph G {\n",
    "  # Graph options\n",
    "  graph [fontname = \"Arial\", rankdir = TB]\n",
    "  node [fontname = \"Arial\", shape = box, style = filled]\n",
    "  edge [fontname = \"Arial\"]\n\n",
    
    "  # Input node\n",
    "  input [label = \"Input Data\", fillcolor = \"#E8F8F5\", width = 2, height = 0.8]\n\n",
    
    "  # Function node\n",
    "  func [label = \"", func_name, "()\", fillcolor = \"#D4EFDF\", fontsize = 14, width = 2.5, height = 1]\n\n",
    
    "  # Connect input to function\n",
    "  input -> func [color = \"#2C3E50\", penwidth = 1.5]\n\n"
  )
  
  # Add function-specific nodes
  if (is_dplyr) {
    dot_code <- paste0(
      dot_code,
      "  # Dplyr specific nodes\n",
      "  filter_criteria [label = \"Apply Filtering Criteria\", fillcolor = \"#D6EAF8\"]\n",
      "  evaluate_expr [label = \"Evaluate Expressions\", fillcolor = \"#D6EAF8\"]\n",
      "  output [label = \"Filtered Data\", fillcolor = \"#E8F8F5\", width = 2, height = 0.8]\n\n",
      
      "  # Connect dplyr flow\n",
      "  func -> filter_criteria [color = \"#2C3E50\"]\n",
      "  filter_criteria -> evaluate_expr [color = \"#2C3E50\"]\n",
      "  evaluate_expr -> output [color = \"#2C3E50\"]\n\n"
    )
  } else if (is_ggplot) {
    dot_code <- paste0(
      dot_code,
      "  # ggplot specific nodes\n",
      "  aesthetics [label = \"Map Data to Aesthetics\", fillcolor = \"#D6EAF8\"]\n",
      "  geometries [label = \"Apply Geometric Elements\", fillcolor = \"#D6EAF8\"]\n",
      "  styling [label = \"Apply Styling\", fillcolor = \"#D6EAF8\"]\n",
      "  output [label = \"ggplot Object\", fillcolor = \"#E8F8F5\", width = 2, height = 0.8]\n\n",
      
      "  # Connect ggplot flow\n",
      "  func -> aesthetics [color = \"#2C3E50\"]\n",
      "  aesthetics -> geometries [color = \"#2C3E50\"]\n",
      "  geometries -> styling [color = \"#2C3E50\"]\n",
      "  styling -> output [color = \"#2C3E50\"]\n\n"
    )
  } else if (is_stats) {
    dot_code <- paste0(
      dot_code,
      "  # stats specific nodes\n",
      "  model_data [label = \"Prepare Model Data\", fillcolor = \"#D6EAF8\"]\n",
      "  fit_model [label = \"Fit Statistical Model\", fillcolor = \"#D6EAF8\"]\n",
      "  diagnostics [label = \"Compute Diagnostics\", fillcolor = \"#D6EAF8\"]\n",
      "  output [label = \"Model Object\", fillcolor = \"#E8F8F5\", width = 2, height = 0.8]\n\n",
      
      "  # Connect stats flow\n",
      "  func -> model_data [color = \"#2C3E50\"]\n",
      "  model_data -> fit_model [color = \"#2C3E50\"]\n",
      "  fit_model -> diagnostics [color = \"#2C3E50\"]\n",
      "  diagnostics -> output [color = \"#2C3E50\"]\n\n"
    )
  } else {
    dot_code <- paste0(
      dot_code,
      "  # Generic function nodes\n",
      "  process_inputs [label = \"Process Function Inputs\", fillcolor = \"#D6EAF8\"]\n",
      "  execute_logic [label = \"Execute Core Logic\", fillcolor = \"#D6EAF8\"]\n",
      "  output [label = \"Return Value\", fillcolor = \"#E8F8F5\", width = 2, height = 0.8]\n\n",
      
      "  # Connect generic flow\n",
      "  func -> process_inputs [color = \"#2C3E50\"]\n",
      "  process_inputs -> execute_logic [color = \"#2C3E50\"]\n",
      "  execute_logic -> output [color = \"#2C3E50\"]\n\n"
    )
  }
  
  # Close the graph
  dot_code <- paste0(dot_code, "}\n")
  
  # Create and return the diagram
  diagram <- DiagrammeR::grViz(dot_code)
  return(diagram)
}

# Function Network visualization
generate_function_network <- function(func_name, metadata) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generating function network for ", func_name)
  }
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    message("visNetwork package not available, using ASCII fallback")
    return(generate_function_network_ascii(func_name, metadata))
  }
  
  # Extract metadata
  pkg <- metadata$package %||% "Unknown"
  
  # Get related functions
  related_funcs <- get_related_functions(func_name, pkg)
  
  # If no related functions found, use generic placeholders
  if (length(related_funcs) == 0) {
    related_funcs <- c(
      paste0("related_", func_name, "_1"),
      paste0("related_", func_name, "_2"),
      paste0("related_", func_name, "_3"),
      paste0("related_", func_name, "_4"),
      paste0("related_", func_name, "_5")
    )
  }
  
  # Create nodes for the function network
  nodes <- data.frame(
    id = 1:(length(related_funcs) + 1),
    label = c(func_name, related_funcs),
    group = c("main", rep("related", length(related_funcs))),
    title = c(paste0("Main function: ", func_name), 
             paste0("Related function in ", pkg, " package: ", related_funcs)),
    shape = c("box", rep("ellipse", length(related_funcs))),
    shadow = c(TRUE, rep(FALSE, length(related_funcs))),
    color.background = c("#D4EFDF", rep("#E8F8F5", length(related_funcs))),
    color.border = c("#2C3E50", rep("#5D6D7E", length(related_funcs))),
    borderWidth = c(3, rep(1, length(related_funcs)))
  )
  
  # Create edges connecting related functions
  edges <- data.frame(
    from = rep(1, length(related_funcs)),
    to = 2:(length(related_funcs) + 1),
    arrows = "to",
    smooth = TRUE,
    color = "#2C3E50",
    width = 2
  )
  
  # Create the network visualization with interactive options
  network <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "400px") %>%
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visNetwork::visLayout(randomSeed = 123) %>% # Consistent layout
    visNetwork::visPhysics(solver = "forceAtlas2Based", 
               forceAtlas2Based = list(gravitationalConstant = -50, 
                                      centralGravity = 0.01, 
                                      springLength = 100, 
                                      springConstant = 0.08))
  
  return(network)
}

# Code Highlight visualization
generate_code_highlight <- function(func_name, metadata) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Generating code highlight for ", func_name)
  }
  
  if (!requireNamespace("highlight", quietly = TRUE) || !requireNamespace("htmltools", quietly = TRUE)) {
    message("highlight or htmltools package not available, using ASCII fallback")
    return(generate_code_highlight_ascii(func_name, metadata))
  }
  
  # Extract metadata
  pkg <- metadata$package %||% "Unknown"
  args <- metadata$args %||% character(0)
  args_str <- paste(args, collapse = ", ")
  
  # Try to get actual function source code
  func_source <- get_function_source(func_name, pkg, args_str)
  
  # If we couldn't get the source, use our predefined examples
  if (is.null(func_source)) {
    # Customize source code based on known packages
    if (pkg == "dplyr" && func_name == "filter") {
      func_source <- 'function(.data, ..., .by = NULL, .preserve = FALSE) {
  # Filter rows that match the given conditions
  # from package: dplyr

  if (!is.null(.by)) {
    # Group by specified columns first
    tmp <- group_by(.data, !!!.by)
    on.exit(unbind_groups(tmp))
  } else {
    tmp <- .data
  }

  # Evaluate filter conditions
  dots <- enquos(...)
  result <- filter_eval(tmp, dots, caller_env())

  # Preserve grouping structure if requested
  if (.preserve && is_grouped_df(.data)) {
    result <- preserve_grouping(result, .data)
  }

  return(result)
}'
    } else if (pkg == "ggplot2" && func_name == "ggplot") {
      func_source <- 'function(data = NULL, mapping = aes(), ..., environment = parent.frame()) {
  # Create a new ggplot object
  # from package: ggplot2
  
  # Create the plot object
  p <- structure(list(
    data = data,
    layers = list(),
    scales = scales_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot"))

  # Add any additional parameters
  p <- ggplot_add(dots(...), p, list(...))
  
  # Assign default theme
  p$theme <- theme_get()
  
  # Set options and return
  set_last_plot(p)
  return(p)
}'
    } else if (pkg == "stats" && func_name == "lm") {
      func_source <- 'function(formula, data, subset, weights, na.action, method = "qr",
         model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
         contrasts = NULL, offset, ...) {
  # Fit linear models
  # from package: stats
  
  # Process call and formula
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  # Extract components
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  
  # Fit the model
  x <- model.matrix(mt, mf, contrasts)
  z <- lm.fit(x, y, offset, singular.ok, ...)
  
  # Set class and attributes
  z$terms <- mt
  z$call <- cl
  z$na.action <- attr(mf, "na.action")
  z$xlevels <- .getXlevels(mt, mf)
  class(z) <- c(if(isS) "mlm", "lm")
  z
}'
    } else {
      # Default generic function body
      func_source <- paste0('function(', args_str, ') {
  # Function implementation for ', func_name, '
  # From package: ', pkg, '
  # This is a placeholder function body
  # that would normally contain the real code
  
  # Process inputs
  args <- list(...)
  
  # Execute function logic
  result <- process_data(data)
  
  # Return output
  return(result)
}')
    }
  }
  
  # Create a temporary file with the source code - this is the most reliable method
  temp_file <- tempfile(fileext = ".R")
  writeLines(func_source, temp_file)
  
  # Use options to suppress the warning message temporarily
  # This suppresses a warning from highlight::simple_detective function 
  # that uses sprintf with a $ character and ignore.case incorrectly
  # The warning message is "one argument not used by format '%s$'"$'"
  old_warn <- options(warn = -1)  # Suppress warnings
  on.exit(options(old_warn))      # Restore warning level when function exits
  
  # Create highlighted HTML using the file with explicit file parameter
  highlighted <- highlight::highlight(file = temp_file, renderer = highlight::renderer_html())
  html <- htmltools::HTML(highlighted)
  
  # Clean up the temporary file
  unlink(temp_file)
  
  # Wrap in a div for styling
  output <- htmltools::div(
    style = "background-color: #f8f8f8; padding: 10px; border-radius: 5px; max-height: 400px; overflow: auto;",
    htmltools::h4(paste0("Source code for ", func_name, " in package ", pkg)),
    html
  )
  
  return(output)
}

# ASCII fallback functions
generate_ascii_data_flow <- function(func_name, metadata) {
  message("Using ASCII fallback for data flow diagram")
  
  ascii <- paste0(
    "┌───────────┐     ┌───────────┐     ┌───────────┐\n",
    "│ Input Data│────>│ ", sprintf("%-9s", paste0(func_name, "()")), "│────>│ Output Data│\n",
    "└───────────┘     └───────────┘     └───────────┘"
  )
  
  return(ascii)
}

generate_function_network_ascii <- function(func_name, metadata) {
  message("Using ASCII fallback for function network")
  
  ascii <- paste0(
    "         ┌───────────────┐\n",
    "         │ ", sprintf("%-13s", func_name), "│\n",
    "         └───────┬───────┘\n",
    "          ┌──────┴───────┐\n",
    "          │              │\n",
    "┌─────────▼──────┐ ┌─────▼──────────┐\n",
    "│ Related Func 1 │ │ Related Func 2 │\n",
    "└────────────────┘ └────────────────┘"
  )
  
  return(ascii)
}

generate_code_highlight_ascii <- function(func_name, metadata) {
  message("Using ASCII fallback for code highlight")
  
  # Create a simple formatted code display
  args_str <- paste(metadata$args, collapse = ", ")
  ascii <- paste0(
    "function(", args_str, ") {\n",
    "  # Function implementation for ", func_name, "\n",
    "  # ...\n",
    "}"
  )
  
  return(ascii)
}

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}