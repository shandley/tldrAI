# Simple visualizations for the tldrAI package

#' Generate ASCII visualizations for different visualization types
#'
#' @param func_name Name of the function
#' @param metadata List containing function metadata
#' @param vis_type Type of visualization (diagram, flowchart, data_flow, etc.)
#' @return ASCII representation of the visualization
#' @export
generate_ascii_visualization <- function(func_name, metadata, vis_type) {
  if (vis_type == "diagram") {
    return(generate_ascii_diagram(func_name, metadata))
  } else if (vis_type == "flowchart") {
    return(generate_ascii_flowchart(func_name, metadata))
  } else if (vis_type == "data_flow") {
    return(generate_ascii_data_flow(func_name, metadata))
  } else if (vis_type == "function_network") {
    return(generate_ascii_function_network(func_name, metadata))
  } else if (vis_type == "code_highlight") {
    return(generate_ascii_code_highlight(func_name, metadata))
  } else {
    return(paste("ASCII Visualization for", func_name, "of type", vis_type))
  }
}

# Helper function to create a diagram visualization
generate_ascii_diagram <- function(func_name, metadata) {
  args <- metadata$args %||% character(0)
  pkg <- metadata$package %||% "Unknown"
  
  # Create a simple ASCII box
  width <- max(nchar(func_name) + 2, 20)
  top <- paste0("╭", paste(rep("─", width), collapse = ""), "╮")
  bottom <- paste0("╰", paste(rep("─", width), collapse = ""), "╯")
  
  # Function name
  name_line <- paste0("│ ", func_name, paste(rep(" ", width - nchar(func_name) - 2), collapse = ""), "│")
  
  # Package
  pkg_line <- paste0("│ Package: ", pkg, paste(rep(" ", width - nchar(pkg) - 11), collapse = ""), "│")
  
  # Arguments
  args_header <- "│ Arguments:                     │"
  args_lines <- character(0)
  for (arg in args) {
    if (nchar(arg) > width - 6) {
      arg <- paste0(substr(arg, 1, width - 9), "...")
    }
    args_lines <- c(args_lines, paste0("│   ", arg, paste(rep(" ", width - nchar(arg) - 4), collapse = ""), "│"))
  }
  
  # Combine all parts
  diagram <- c(top, name_line, pkg_line, args_header, args_lines, bottom)
  return(paste(diagram, collapse = "\n"))
}

# Helper function to create a flowchart visualization
generate_ascii_flowchart <- function(func_name, metadata) {
  args <- metadata$args %||% character(0)
  pkg <- metadata$package %||% "Unknown"
  
  # Create start box
  start <- paste0(
    "       ╭─────────╮\n",
    "       │  Start  │\n",
    "       ╰────┬────╯\n",
    "            │\n",
    "            ▼\n"
  )
  
  # Create function box
  func_box <- paste0(
    "    ╭───────────────╮\n",
    "    │ ", sprintf("%-13s", func_name), "│\n",
    "    ╰───────┬───────╯\n",
    "            │\n",
    "            ▼\n"
  )
  
  # Create process boxes
  process <- paste0(
    "╭─────────────────────╮\n",
    "│ Process Input Data   │\n",
    "╰──────────┬──────────╯\n",
    "           │\n",
    "           ▼\n"
  )
  
  # Create conditional if needed
  conditional <- ""
  if (any(grepl("if|condition|test|check", args, ignore.case = TRUE))) {
    conditional <- paste0(
      "    ┌──────────────┐\n",
      "    │  Condition?  │\n",
      "    └───────┬──────┘\n",
      "            │\n",
      "      ┌─────┴─────┐\n",
      "      │           │\n",
      "      ▼           ▼\n",
      " ┌───────┐   ┌───────┐\n",
      " │  Yes  │   │  No   │\n",
      " └───┬───┘   └───┬───┘\n",
      "     │           │\n",
      "     └─────┬─────┘\n",
      "           │\n",
      "           ▼\n"
    )
  }
  
  # Create loop if needed
  loop <- ""
  if (any(grepl("loop|for|while|each|apply", func_name, ignore.case = TRUE)) || 
      any(grepl("loop|for|while|each|apply", args, ignore.case = TRUE))) {
    loop <- paste0(
      "    ┌──────────────┐\n",
      "    │  More items? │\n",
      "    └───────┬──────┘\n",
      "            │\n",
      "      ┌─────┴─────┐\n",
      "      │           │\n",
      "      ▼           ▼\n",
      " ┌───────┐   ┌───────┐\n",
      " │  Yes  │   │  No   │\n",
      " └───┬───┘   └───┬───┘\n",
      "     │           │\n",
      " ┌───┘           │\n",
      " │               │\n",
      " └───────┐       │\n",
      "         │       │\n",
      "         └───┬───┘\n",
      "             │\n",
      "             ▼\n"
    )
  }
  
  # Create result and end
  result_end <- paste0(
    "╭─────────────────────╮\n",
    "│ Return Result       │\n",
    "╰──────────┬──────────╯\n",
    "           │\n",
    "           ▼\n",
    "       ╭─────────╮\n",
    "       │   End   │\n",
    "       ╰─────────╯\n"
  )
  
  # Create header and footer
  header <- paste0("╭─ Flowchart: ", func_name, " (", pkg, ") ─╮\n")
  footer <- paste0("╰", paste(rep("─", nchar(header) - 3), collapse = ""), "╯\n")
  
  # Combine to create the flowchart
  if (!any(grepl("loop|for|while|each|apply", func_name, ignore.case = TRUE)) && 
      !any(grepl("loop|for|while|each|apply", args, ignore.case = TRUE)) && 
      !any(grepl("if|condition|test|check", args, ignore.case = TRUE))) {
    # Basic flowchart
    viz <- paste0(
      header,
      start,
      func_box,
      process, 
      result_end,
      footer
    )
  } else if (!any(grepl("loop|for|while|each|apply", func_name, ignore.case = TRUE)) && 
             !any(grepl("loop|for|while|each|apply", args, ignore.case = TRUE))) {
    # Conditional but no loop
    viz <- paste0(
      header,
      start,
      func_box,
      process, 
      conditional,
      result_end,
      footer
    )
  } else if (!any(grepl("if|condition|test|check", args, ignore.case = TRUE))) {
    # Loop but no conditional
    viz <- paste0(
      header,
      start,
      func_box,
      process, 
      loop,
      result_end,
      footer
    )
  } else {
    # Both conditional and loop
    viz <- paste0(
      header,
      start,
      func_box,
      process,
      conditional,
      loop,
      result_end,
      footer
    )
  }
  
  return(viz)
}

# Helper function to create a data flow diagram visualization
generate_ascii_data_flow <- function(func_name, metadata) {
  pkg <- metadata$package %||% "Unknown"
  args <- metadata$args %||% character(0)
  
  # Determine function type for customization
  is_dplyr <- pkg %in% c("dplyr", "tidyr", "purrr")
  is_ggplot <- pkg %in% c("ggplot2", "ggvis")
  is_stats <- pkg %in% c("stats", "lme4", "caret")
  
  # Create header
  header <- paste0(
    "╭─ Data Flow Diagram: ", func_name, " (", pkg, ") ─╮\n"
  )
  
  # Create input section
  inputs <- "│ Inputs:                               │\n"
  if (length(args) > 0) {
    for (arg in args) {
      if (nchar(arg) > 35) {
        arg <- paste0(substr(arg, 1, 32), "...")
      }
      inputs <- paste0(inputs, "│   ● ", sprintf("%-33s", arg), "│\n")
    }
  }
  
  # Create processing section
  processing <- "│                                     │\n│ Processing:                          │\n"
  
  if (is_dplyr) {
    processing <- paste0(processing,
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 1. Apply filtering criteria │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 2. Evaluate expressions     │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 3. Return matching rows     │   │\n",
      "│   └───────────┬─────────────────┘   │\n"
    )
  } else if (is_ggplot) {
    processing <- paste0(processing,
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 1. Map data to aesthetics   │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 2. Apply geometric elements │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 3. Apply styling            │   │\n",
      "│   └───────────┬─────────────────┘   │\n"
    )
  } else if (is_stats) {
    processing <- paste0(processing,
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 1. Prepare model data       │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 2. Fit statistical model    │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 3. Compute diagnostics      │   │\n",
      "│   └───────────┬─────────────────┘   │\n"
    )
  } else {
    processing <- paste0(processing,
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 1. Process function inputs  │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 2. Execute core logic       │   │\n",
      "│   └───────────┬─────────────────┘   │\n",
      "│               │                     │\n",
      "│               ▼                     │\n",
      "│   ┌─────────────────────────────┐   │\n",
      "│   │ 3. Prepare output           │   │\n",
      "│   └───────────┬─────────────────┘   │\n"
    )
  }
  
  # Create output section
  output_type <- "filtered data frame"
  if (is_ggplot) {
    output_type <- "ggplot object"
  } else if (is_stats) {
    output_type <- "model object"
  }
  
  outputs <- paste0(
    "│               │                     │\n",
    "│               ▼                     │\n",
    "│ Outputs:                            │\n",
    "│   ● ", sprintf("%-33s", output_type), "│\n"
  )
  
  # Create footer
  footer <- paste0("╰─────────────────────────────────────╯\n")
  
  # Combine all parts
  diagram <- paste0(header, inputs, processing, outputs, footer)
  return(diagram)
}

# Helper function to create a function network visualization
generate_ascii_function_network <- function(func_name, metadata) {
  pkg <- metadata$package %||% "Unknown"
  
  # Create a simple network diagram
  diagram <- paste0(
    "╭─ Function Network: ", func_name, " (", pkg, ") ─╮\n",
    "│                                     │\n",
    "│         ┌───────────────┐          │\n",
    "│         │ ", sprintf("%-13s", func_name), "│          │\n",
    "│         └───────┬───────┘          │\n",
    "│                 │                  │\n",
    "│          ┌──────┴───────┐          │\n",
    "│          │              │          │\n",
    "│  ┌───────▼──────┐ ┌─────▼──────┐   │\n",
    "│  │ Related Func1│ │Related Func2│   │\n",
    "│  └───────┬──────┘ └──────┬─────┘   │\n",
    "│          │               │         │\n",
    "│     ┌────▼────┐     ┌────▼────┐    │\n",
    "│     │  Func3  │     │  Func4  │    │\n",
    "│     └─────────┘     └─────────┘    │\n",
    "│                                     │\n",
    "╰─────────────────────────────────────╯\n"
  )
  
  return(diagram)
}

# Helper function to create a code highlight visualization
generate_ascii_code_highlight <- function(func_name, metadata) {
  args_str <- paste(metadata$args %||% character(0), collapse = ", ")
  
  # Create a simple code display with syntax highlighting represented by ASCII
  diagram <- paste0(
    "╭─ Code Highlight: ", func_name, " ─╮\n",
    "│                              │\n",
    "│  \033[34mfunction\033[0m(\033[33m", args_str, "\033[0m) {     │\n",
    "│    \033[35m# Function implementation\033[0m  │\n",
    "│    \033[31m# Core logic\033[0m               │\n",
    "│    \033[32m...\033[0m                        │\n",
    "│  }                           │\n",
    "│                              │\n",
    "╰──────────────────────────────╯\n"
  )
  
  # Note: Terminal colors may not display correctly in all environments
  # So let's create a plain fallback version too
  plain_diagram <- paste0(
    "╭─ Code Highlight: ", func_name, " ─╮\n",
    "│                              │\n",
    "│  function(", args_str, ") {     │\n",
    "│    # Function implementation  │\n",
    "│    # Core logic               │\n",
    "│    ...                        │\n",
    "│  }                           │\n",
    "│                              │\n",
    "╰──────────────────────────────╯\n"
  )
  
  # Use plain version for now to ensure compatibility
  return(plain_diagram)
}

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}