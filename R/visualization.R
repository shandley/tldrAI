#' Visualization handler for tldrAI
#'
#' @description R6 Class for generating and handling visualizations
#'
#' @importFrom R6 R6Class
#' @importFrom utils packageVersion
#' @keywords internal
VisualizationHandler <- R6::R6Class("VisualizationHandler",
  public = list(
    #' @field visualization_data A list containing visualization data
    visualization_data = NULL,
    
    #' @field supported_vis_types List of supported visualization types
    supported_vis_types = c("diagram", "flowchart"),
    
    #' @description Initialize a new VisualizationHandler
    #' @param settings List of visualization settings
    initialize = function(settings = NULL) {
      self$visualization_data <- list(
        vis_type = NULL,
        func_name = NULL,
        package = NULL,
        args = NULL,
        returns = NULL,
        diagram = NULL
      )
      
      invisible(self)
    },
    
    #' @description Check if required visualization packages are available
    #' @param type The type of visualization to check packages for
    #' @param prompt_install Logical indicating whether to prompt for installation if packages are missing
    #' @return Logical indicating if the required packages are available
    check_packages = function(type = "diagram", prompt_install = FALSE) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Checking packages for visualization type: ", type)
      }
      
      required_packages <- self$get_required_packages(type)
      
      # Check each required package
      missing_packages <- character(0)
      for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          if (get_config("debug_mode", default = FALSE)) {
            message("DEBUG: Package not available: ", pkg)
          }
          missing_packages <- c(missing_packages, pkg)
        }
      }
      
      # If all packages are available, return TRUE
      if (length(missing_packages) == 0) {
        return(TRUE)
      }
      
      # Handle missing packages
      missing_pkg_str <- paste(missing_packages, collapse = ", ")
      
      # Check auto_install configuration
      vis_settings <- get_config("visualization_settings", default = list(auto_install = FALSE))
      auto_install <- vis_settings$auto_install
      if (is.null(auto_install)) auto_install <- FALSE
      
      # Handle auto installation if enabled
      if (auto_install) {
        message(paste0(
          "📦 The following packages are required for visualization and will be installed automatically: ", 
          missing_pkg_str
        ))
        
        install_success <- tryCatch({
          for (pkg in missing_packages) {
            utils::install.packages(pkg)
          }
          TRUE
        }, error = function(e) {
          message("❌ Error installing packages: ", e$message)
          FALSE
        })
        
        # Verify installation
        if (install_success) {
          still_missing <- character(0)
          for (pkg in missing_packages) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
              still_missing <- c(still_missing, pkg)
            }
          }
          
          if (length(still_missing) == 0) {
            message("✅ All required packages were successfully installed!")
            return(TRUE)
          } else {
            message("⚠️ Some packages could not be installed. Visualization will be limited.")
          }
        }
      } else if (prompt_install) {
        # Interactive prompt for installation
        message(paste0(
          "\n📦 Visualization requires the following package(s): ", 
          missing_pkg_str
        ))
        
        response <- utils::menu(
          c("Yes", "Yes, and remember this choice", "No"), 
          title = "Would you like to install these packages now?"
        )
        
        if (response == 1 || response == 2) {
          # Install packages
          install_success <- tryCatch({
            for (pkg in missing_packages) {
              utils::install.packages(pkg)
            }
            TRUE
          }, error = function(e) {
            message("❌ Error installing packages: ", e$message)
            FALSE
          })
          
          # If user chose to remember this choice, update configuration
          if (response == 2 && install_success) {
            message("✅ Updating configuration to automatically install packages in the future")
            tldr_visualization_config(auto_install = TRUE)
          }
          
          # Verify installation
          still_missing <- character(0)
          for (pkg in missing_packages) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
              still_missing <- c(still_missing, pkg)
            }
          }
          
          if (length(still_missing) == 0) {
            message("✅ All required packages were successfully installed!")
            return(TRUE)
          } else {
            message("⚠️ Some packages could not be installed. Visualization will be limited.")
          }
        } else {
          # User declined installation
          message(paste0(
            "\n📝 To manually install these packages later, run:\n",
            "install.packages(c(\"", paste(missing_packages, collapse = "\", \""), "\"))\n\n",
            "Or enable automatic installation with:\n",
            "tldr_visualization_config(auto_install = TRUE)"
          ))
        }
      } else {
        # Just provide information without prompting
        message(paste0(
          "\n📦 Visualization requires the following package(s): ", 
          missing_pkg_str, 
          "\n📝 To install these packages, run:\n",
          "install.packages(c(\"", paste(missing_packages, collapse = "\", \""), "\"))\n\n",
          "Or enable automatic installation with:\n",
          "tldr_visualization_config(auto_install = TRUE)"
        ))
      }
      
      FALSE
    },
    
    #' @description Get the required packages for a visualization type
    #' @param type The type of visualization
    #' @return Character vector of required package names
    get_required_packages = function(type = "diagram") {
      if (type == "diagram") {
        return(c("DiagrammeR"))
      } else if (type == "flowchart") {
        return(c("DiagrammeR"))
      } else {
        return(character(0))
      }
    },
    
    #' @description Generate a visualization for a function
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @param vis_type The type of visualization to generate
    #' @param prompt_install Logical indicating whether to prompt for installation if packages are missing
    #' @return A visualization object, ASCII fallback, or NULL if not available
    generate_visualization = function(func_name, metadata, vis_type = "diagram", prompt_install = TRUE) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Generating visualization for function: ", func_name)
        message("DEBUG: Visualization type: ", vis_type)
      }
      
      # Store function information
      self$visualization_data$func_name <- func_name
      self$visualization_data$package <- metadata$package
      self$visualization_data$args <- metadata$args
      self$visualization_data$returns <- metadata$returns
      
      # Validate visualization type
      if (!vis_type %in% self$supported_vis_types) {
        warning("Unsupported visualization type: ", vis_type, ". Supported types are: ", 
                paste(self$supported_vis_types, collapse = ", "))
        return(NULL)
      }
      
      # Check if required packages are available
      packages_available <- self$check_packages(vis_type, prompt_install = prompt_install)
      
      # If packages aren't available, generate a fallback ASCII visualization
      if (!packages_available) {
        fallback_vis <- self$generate_fallback_visualization(func_name, metadata, vis_type)
        self$visualization_data$diagram <- fallback_vis
        self$visualization_data$vis_type <- paste0("fallback_", vis_type)
        return(fallback_vis)
      }
      
      # Generate visualization based on type
      vis_obj <- NULL
      tryCatch({
        if (vis_type == "diagram") {
          vis_obj <- self$generate_function_diagram(func_name, metadata)
        } else if (vis_type == "flowchart") {
          vis_obj <- self$generate_function_flowchart(func_name, metadata)
        }
        
        # Store the visualization
        self$visualization_data$diagram <- vis_obj
        self$visualization_data$vis_type <- vis_type
        
        vis_obj
      }, error = function(e) {
        warning("Failed to generate visualization: ", e$message)
        # On error, fall back to ASCII visualization
        fallback_vis <- self$generate_fallback_visualization(func_name, metadata, vis_type)
        self$visualization_data$diagram <- fallback_vis
        self$visualization_data$vis_type <- paste0("fallback_", vis_type)
        fallback_vis
      })
    },
    
    #' @description Generate a function diagram visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A DiagrammeR graph object or NULL if not available
    generate_function_diagram = function(func_name, metadata) {
      if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
        return(NULL)
      }
      
      # Extract arguments
      args <- metadata$args
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Generating function diagram for: ", func_name)
        message("DEBUG: Arguments: ", paste(args, collapse = ", "))
      }
      
      # Create function diagram with DiagrammeR
      tryCatch({
        # Create dot language for the graph
        nodes <- paste0(
          "digraph func_diagram {\n",
          "  graph [rankdir=LR, fontname=Arial, fontsize=12, dpi=300]\n",
          "  node [shape=box, style=filled, fillcolor=lightblue, fontname=Arial, fontsize=10]\n",
          "  edge [fontname=Arial, fontsize=9]\n",
          
          # Add nodes for inputs (arguments)
          paste0("  arg", seq_along(args), " [label=\"", args, "\", shape=oval, fillcolor=\"#E6F3FF\"]\n", collapse = ""),
          
          # Add function node
          "  func [label=\"", func_name, "\", shape=box, fillcolor=\"#94C9FF\"]\n",
          
          # Add output node
          "  output [label=\"Return Value\", shape=oval, fillcolor=\"#E6FFE6\"]\n",
          
          # Add edges from arguments to function
          paste0("  arg", seq_along(args), " -> func\n", collapse = ""),
          
          # Add edge from function to output
          "  func -> output\n",
          "}\n"
        )
        
        # Create graph using DiagrammeR
        graph <- DiagrammeR::grViz(nodes)
        return(graph)
      }, error = function(e) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Error generating function diagram: ", e$message)
        }
        return(NULL)
      })
    },
    
    #' @description Generate a function flowchart visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A DiagrammeR graph object or NULL if not available
    generate_function_flowchart = function(func_name, metadata) {
      if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
        return(NULL)
      }
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Generating function flowchart for: ", func_name)
      }
      
      # Try to extract some basic information about the function
      pkg <- metadata$package
      body_summary <- metadata$body_summary %||% ""
      
      # Simplistic flow detection (this is a basic implementation)
      has_conditional <- grepl("if\\s*\\(|else\\s*\\{", body_summary)
      has_loop <- grepl("for\\s*\\(|while\\s*\\(", body_summary)
      
      # Create flowchart with DiagrammeR
      tryCatch({
        # Create dot language for the graph
        nodes <- paste0(
          "digraph flowchart {\n",
          "  graph [rankdir=TB, fontname=Arial, fontsize=12, dpi=300]\n",
          "  node [shape=box, style=filled, fillcolor=lightblue, fontname=Arial, fontsize=10]\n",
          "  edge [fontname=Arial, fontsize=9]\n",
          
          # Start node
          "  start [label=\"Start\", shape=oval, fillcolor=\"#E6F3FF\"]\n",
          
          # Processing steps
          "  process [label=\"Process Inputs\", shape=box, fillcolor=\"#94C9FF\"]\n"
        )
        
        # Add conditional nodes if detected
        if (has_conditional) {
          nodes <- paste0(nodes,
            "  conditional [label=\"Conditional Logic\", shape=diamond, fillcolor=\"#FFE6CC\"]\n",
            "  process -> conditional\n",
            "  conditional -> result [label=\"  True\"]\n",
            "  conditional -> alt_result [label=\"  False\"]\n",
            "  alt_result [label=\"Alternative Result\", fillcolor=\"#FFD6D6\"]\n"
          )
        } else {
          nodes <- paste0(nodes, "  process -> result\n")
        }
        
        # Add loop nodes if detected
        if (has_loop) {
          nodes <- paste0(nodes,
            "  loop [label=\"Iteration/Loop\", shape=box, fillcolor=\"#CCFFCC\"]\n",
            "  process -> loop\n",
            "  loop -> loop [label=\"  Next iteration\"]\n",
            "  loop -> result [label=\"  Loop completed\"]\n"
          )
        }
        
        # Result and end nodes
        nodes <- paste0(nodes,
          "  result [label=\"Generate Result\", fillcolor=\"#E6FFE6\"]\n",
          "  end [label=\"Return\", shape=oval, fillcolor=\"#E6F3FF\"]\n",
          "  start -> process\n"
        )
        
        # If no conditional or loop was added, ensure there's a path to result
        if (!has_conditional && !has_loop) {
          nodes <- paste0(nodes, "  process -> result\n")
        }
        
        # Add final edge to end
        nodes <- paste0(nodes, "  result -> end\n")
        
        # Add alternative path to end if there's a conditional
        if (has_conditional) {
          nodes <- paste0(nodes, "  alt_result -> end\n")
        }
        
        # Close the graph
        nodes <- paste0(nodes, "}\n")
        
        # Create graph using DiagrammeR
        graph <- DiagrammeR::grViz(nodes)
        return(graph)
      }, error = function(e) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Error generating function flowchart: ", e$message)
        }
        return(NULL)
      })
    },
    
    #' @description Get SVG code for the visualization
    #' @return Character string with SVG code or NULL if not available
    get_svg = function() {
      vis <- self$visualization_data$diagram
      if (is.null(vis)) {
        return(NULL)
      }
      
      tryCatch({
        if (requireNamespace("DiagrammeR", quietly = TRUE) && 
            inherits(vis, "htmlwidget")) {
          # Convert the DiagrammeR graph to SVG
          # This is a simplified approach - in a real implementation, 
          # this would handle extracting the SVG from the widget
          return("<svg>Visualization would be here</svg>")
        }
        NULL
      }, error = function(e) {
        NULL
      })
    },
    
    #' @description Print the visualization
    #' @return Invisibly returns self
    print = function() {
      vis <- self$visualization_data$diagram
      if (!is.null(vis)) {
        print(vis)
      } else {
        message("No visualization available.")
      }
      invisible(self)
    },
    
    #' @description Generate a fallback ASCII visualization when packages are missing
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @param vis_type The requested visualization type
    #' @return A character string with ASCII visualization
    generate_fallback_visualization = function(func_name, metadata, vis_type = "diagram") {
      # Create a simple ASCII art visualization based on the requested type
      if (vis_type == "diagram") {
        return(self$generate_ascii_diagram(func_name, metadata))
      } else if (vis_type == "flowchart") {
        return(self$generate_ascii_flowchart(func_name, metadata))
      } else {
        # Generic fallback
        return(paste0(
          "Function: ", func_name, "\n",
          "Package: ", metadata$package %||% "Unknown", "\n",
          "Arguments: ", paste(metadata$args %||% "None", collapse = ", "), "\n"
        ))
      }
    },
    
    #' @description Generate an ASCII function diagram
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A character string with ASCII diagram
    generate_ascii_diagram = function(func_name, metadata) {
      args <- metadata$args %||% character(0)
      pkg <- metadata$package %||% "Unknown"
      
      # Create header
      header <- paste0(
        "╭─ Function Diagram: ", func_name, " (", pkg, ") ─╮\n"
      )
      
      # Create the middle section with arguments
      args_section <- ""
      if (length(args) > 0) {
        args_section <- "│ Arguments:                          │\n"
        for (arg in args) {
          if (nchar(arg) > 35) {
            arg <- paste0(substr(arg, 1, 32), "...")
          }
          args_section <- paste0(args_section, "│   ● ", sprintf("%-33s", arg), "│\n")
        }
      } else {
        args_section <- "│ Arguments: None                     │\n"
      }
      
      # Function box
      func_box <- paste0(
        "│                                    │\n",
        "│           ┌──────────┐             │\n",
        "│           │ ", sprintf("%-8s", func_name), " │             │\n",
        "│           └──────────┘             │\n",
        "│                                    │\n"
      )
      
      # Return value
      return_section <- "│ Returns: [Function output]          │\n"
      
      # Footer
      footer <- paste0(
        "╰────────────────────────────────────╯\n\n",
        "Note: This is a text-based visualization.\n",
        "For graphical visualizations, install required packages:\n",
        "install.packages(\"", paste(self$get_required_packages(vis_type = "diagram"), collapse = "\", \""), "\")"
      )
      
      # Combine all parts
      viz <- paste0(
        header,
        args_section,
        func_box,
        return_section,
        footer
      )
      
      return(viz)
    },
    
    #' @description Generate an ASCII function flowchart
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A character string with ASCII flowchart
    generate_ascii_flowchart = function(func_name, metadata) {
      pkg <- metadata$package %||% "Unknown"
      body_summary <- metadata$body_summary %||% ""
      
      # Basic flow detection (simplified version of what's in the graphic version)
      has_conditional <- grepl("if\\s*\\(|else\\s*\\{", body_summary)
      has_loop <- grepl("for\\s*\\(|while\\s*\\(", body_summary)
      
      # Create header
      header <- paste0(
        "╭─ Function Flowchart: ", func_name, " (", pkg, ") ─╮\n"
      )
      
      # Start node
      start <- paste0(
        "│            ┌─────────┐             │\n",
        "│            │  Start  │             │\n",
        "│            └────┬────┘             │\n",
        "│                 │                  │\n",
        "│                 ▼                  │\n"
      )
      
      # Process inputs
      process <- paste0(
        "│         ┌─────────────────┐        │\n",
        "│         │  Process Inputs │        │\n",
        "│         └────────┬────────┘        │\n",
        "│                  │                 │\n"
      )
      
      # Conditional section if detected
      conditional <- ""
      if (has_conditional) {
        conditional <- paste0(
          "│                  ▼                 │\n",
          "│             ┌────────┐             │\n",
          "│             │   If   │             │\n",
          "│             └───┬────┘             │\n",
          "│                 │                  │\n",
          "│        ┌────────┴────────┐         │\n",
          "│        │                 │         │\n",
          "│        ▼                 ▼         │\n",
          "│   ┌────────┐        ┌────────┐    │\n",
          "│   │  True  │        │  False │    │\n",
          "│   └───┬────┘        └───┬────┘    │\n",
          "│       │                 │         │\n",
          "│       └────────┐ ┌──────┘         │\n",
          "│                │ │                │\n"
        )
      }
      
      # Loop section if detected
      loop <- ""
      if (has_loop) {
        loop <- paste0(
          "│                  ▼                 │\n",
          "│          ┌──────────────┐          │\n",
          "│    ┌─────┤     Loop     │          │\n",
          "│    │     └──────┬───────┘          │\n",
          "│    │            │                  │\n",
          "│    │            ▼                  │\n",
          "│    │     ┌─────────────┐           │\n",
          "│    │     │  Iteration  │           │\n",
          "│    │     └──────┬──────┘           │\n",
          "│    │            │                  │\n",
          "│    └────────────┘                  │\n"
        )
      }
      
      # Result and end nodes
      result_end <- paste0(
        "│                  ▼                 │\n",
        "│         ┌────────────────┐         │\n",
        "│         │ Generate Result│         │\n",
        "│         └────────┬───────┘         │\n",
        "│                  │                 │\n",
        "│                  ▼                 │\n",
        "│           ┌────────────┐           │\n",
        "│           │    End     │           │\n",
        "│           └────────────┘           │\n"
      )
      
      # Footer
      footer <- paste0(
        "╰────────────────────────────────────╯\n\n",
        "Note: This is a text-based visualization.\n",
        "For graphical visualizations, install required packages:\n",
        "install.packages(\"", paste(self$get_required_packages(vis_type = "flowchart"), collapse = "\", \""), "\")"
      )
      
      # Combine all parts
      if (!has_conditional && !has_loop) {
        # Simplified flowchart if no conditionals or loops detected
        viz <- paste0(
          header,
          start,
          process,
          "│                  ▼                 │\n",
          result_end,
          footer
        )
      } else if (has_conditional && !has_loop) {
        viz <- paste0(
          header,
          start,
          process,
          conditional,
          result_end,
          footer
        )
      } else if (!has_conditional && has_loop) {
        viz <- paste0(
          header,
          start,
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
          process,
          conditional,
          loop,
          result_end,
          footer
        )
      }
      
      return(viz)
    }
  ),
  
  private = list(
    # Null-coalescing operator as a private helper
    `%||%` = function(x, y) {
      if (is.null(x)) y else x
    }
  )
)

#' Create a visualization for a function
#'
#' @param func_name Character string specifying the name of the function
#' @param metadata List with function metadata
#' @param vis_type Character string specifying the visualization type ("diagram" or "flowchart")
#' @param prompt_install Logical indicating whether to prompt for installation if packages are missing
#'
#' @return A visualization object or NULL if not available
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- get_function_metadata("mean")
#' vis <- create_visualization("mean", metadata)
#' print(vis)
#' 
#' # Without installation prompt
#' vis <- create_visualization("mean", metadata, prompt_install = FALSE)
#' }
create_visualization <- function(func_name, metadata, vis_type = "diagram", prompt_install = TRUE) {
  # Create a visualization handler
  handler <- VisualizationHandler$new()
  
  # Generate the visualization
  vis <- handler$generate_visualization(func_name, metadata, vis_type, prompt_install)
  
  # Return the handler object, which contains the visualization
  handler
}

#' Check if visualization packages are available
#'
#' @param vis_type Character string specifying the visualization type
#'
#' @return Logical indicating if the required packages are available
#' @export
#'
#' @examples
#' \dontrun{
#' check_visualization_packages("diagram")
#' }
check_visualization_packages <- function(vis_type = "diagram") {
  handler <- VisualizationHandler$new()
  handler$check_packages(vis_type)
}

#' Get required packages for visualization
#'
#' @param vis_type Character string specifying the visualization type
#'
#' @return Character vector of required package names
#' @export
#'
#' @examples
#' \dontrun{
#' get_visualization_packages("flowchart")
#' }
get_visualization_packages <- function(vis_type = "diagram") {
  handler <- VisualizationHandler$new()
  handler$get_required_packages(vis_type)
}

#' Print a visualization
#'
#' @param visualization A visualization handler object
#'
#' @return Invisibly returns the visualization handler
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- get_function_metadata("mean")
#' vis <- create_visualization("mean", metadata)
#' print_visualization(vis)
#' }
print_visualization <- function(visualization) {
  if (inherits(visualization, "VisualizationHandler")) {
    visualization$print()
  } else {
    message("Not a valid visualization handler object.")
  }
  invisible(visualization)
}

#' Configure visualization settings
#'
#' @param enable Logical indicating whether to enable visualizations
#' @param default_type Character string specifying the default visualization type
#' @param auto_install Logical indicating whether to automatically install required packages
#'
#' @return Invisibly returns the updated configuration
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_visualization_config(enable = TRUE, default_type = "diagram")
#' }
tldr_visualization_config <- function(enable = NULL, default_type = NULL, auto_install = NULL) {
  # Get current configuration
  config <- get_config_all()
  
  # Initialize visualization settings if they don't exist
  if (is.null(config$visualization_settings)) {
    config$visualization_settings <- list(
      enable_visualization = FALSE,
      default_type = "diagram",
      auto_install = FALSE
    )
  }
  
  # Update with non-NULL values
  if (!is.null(enable)) {
    config$visualization_settings$enable_visualization <- enable
  }
  
  if (!is.null(default_type)) {
    # Validate visualization type
    handler <- VisualizationHandler$new()
    if (!default_type %in% handler$supported_vis_types) {
      warning("Unsupported visualization type: ", default_type, 
              ". Supported types are: ", paste(handler$supported_vis_types, collapse = ", "), 
              ". Using 'diagram' as default.")
      default_type <- "diagram"
    }
    config$visualization_settings$default_type <- default_type
  }
  
  if (!is.null(auto_install)) {
    config$visualization_settings$auto_install <- auto_install
  }
  
  # Save the updated configuration
  save_config(config)
  
  # Report the current settings
  message("Visualization settings:")
  message("  Enable visualization: ", 
          ifelse(config$visualization_settings$enable_visualization, "Yes", "No"))
  message("  Default type: ", config$visualization_settings$default_type)
  message("  Auto-install required packages: ", 
          ifelse(config$visualization_settings$auto_install, "Yes", "No"))
  
  invisible(config)
}

#' Install required visualization packages
#'
#' @param vis_type Character string specifying the visualization type
#' @param auto_enable Logical indicating whether to enable auto-installation in config
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' install_visualization_packages("diagram")
#' 
#' # Install and enable auto-installation for future use
#' install_visualization_packages("flowchart", auto_enable = TRUE)
#' }
install_visualization_packages <- function(vis_type = "diagram", auto_enable = FALSE) {
  handler <- VisualizationHandler$new()
  required_packages <- handler$get_required_packages(vis_type)
  
  if (length(required_packages) == 0) {
    message("No packages required for visualization type: ", vis_type)
    return(invisible(TRUE))
  }
  
  # Check which packages are already installed
  to_install <- character(0)
  already_installed <- character(0)
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      to_install <- c(to_install, pkg)
    } else {
      already_installed <- c(already_installed, pkg)
    }
  }
  
  # Report already installed packages
  if (length(already_installed) > 0) {
    message("✓ Already installed: ", paste(already_installed, collapse = ", "))
  }
  
  # Install missing packages
  if (length(to_install) > 0) {
    message("📦 Installing packages for visualization type '", vis_type, "': ",
            paste(to_install, collapse = ", "))
    
    install_success <- tryCatch({
      for (pkg in to_install) {
        message("   Installing package: ", pkg)
        utils::install.packages(pkg)
      }
      TRUE
    }, error = function(e) {
      message("❌ Error installing packages: ", e$message)
      FALSE
    })
    
    # Verify installation
    if (install_success) {
      still_missing <- character(0)
      for (pkg in to_install) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          still_missing <- c(still_missing, pkg)
        }
      }
      
      if (length(still_missing) == 0) {
        message("✅ All required packages were successfully installed!")
        
        # Auto-enable in config if requested
        if (auto_enable) {
          tldr_visualization_config(auto_install = TRUE)
          message("✅ Auto-installation enabled for future visualization package needs")
        }
        
        invisible(TRUE)
      } else {
        message("⚠️ Some packages could not be installed: ", paste(still_missing, collapse = ", "))
        invisible(FALSE)
      }
    } else {
      invisible(FALSE)
    }
  } else {
    message("✅ All required packages are already installed!")
    
    # Auto-enable in config if requested
    if (auto_enable) {
      tldr_visualization_config(auto_install = TRUE)
      message("✅ Auto-installation enabled for future visualization package needs")
    }
    
    invisible(TRUE)
  }
}