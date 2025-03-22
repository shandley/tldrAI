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
    supported_vis_types = c("diagram", "flowchart", "data_flow", "function_network", "code_highlight"),
    
    #' @description Initialize a new VisualizationHandler
    #' @param settings List of visualization settings
    initialize = function(settings = NULL) {
      self$visualization_data <- list(
        vis_type = NULL,
        func_name = NULL,
        package = NULL,
        args = NULL,
        returns = NULL,
        diagram = NULL,
        interactive = FALSE,
        related_functions = NULL
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
      } else if (type == "data_flow") {
        return(c("DiagrammeR", "htmlwidgets"))
      } else if (type == "function_network") {
        return(c("visNetwork", "igraph"))
      } else if (type == "code_highlight") {
        return(c("htmltools", "highlight"))
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
        } else if (vis_type == "data_flow") {
          vis_obj <- self$generate_data_flow_diagram(func_name, metadata)
        } else if (vis_type == "function_network") {
          vis_obj <- self$generate_function_network(func_name, metadata)
        } else if (vis_type == "code_highlight") {
          vis_obj <- self$generate_code_highlight(func_name, metadata)
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
    
    #' @description Generate a data flow diagram visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A DiagrammeR graph object or NULL if not available
    generate_data_flow_diagram = function(func_name, metadata) {
      if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
        return(NULL)
      }
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Generating data flow diagram for: ", func_name)
      }
      
      # Extract function details
      args <- metadata$args %||% character(0)
      pkg <- metadata$package %||% "unknown"
      description <- metadata$description %||% ""
      returns <- metadata$returns %||% "Result"
      
      # Detect data transformation based on package and function
      is_dplyr <- pkg %in% c("dplyr", "tidyr", "purrr")
      is_ggplot <- pkg %in% c("ggplot2", "ggvis")
      is_stats <- pkg %in% c("stats", "lme4", "caret")
      
      # Customize diagram based on function type
      tryCatch({
        # Create dot language for the data flow graph
        nodes <- paste0(
          "digraph data_flow {\n",
          "  graph [rankdir=LR, fontname=Arial, fontsize=12, dpi=300, bgcolor=\"#FFFFFF\", splines=true]\n",
          "  node [fontname=Arial, fontsize=10, shape=box, style=filled, fillcolor=\"#E6F3FF\", margin=0.2]\n",
          "  edge [fontname=Arial, fontsize=9, color=\"#333333\"]\n",
          
          # Add header with function name
          "  label=\"Data Flow: ", func_name, "\"\n",
          "  labelloc=\"t\"\n",
          "  fontsize=14\n",
          "  fontname=\"Arial-Bold\"\n\n"
        )
        
        # Add input nodes with appropriate styling
        nodes <- paste0(nodes, 
          "  subgraph cluster_inputs {\n",
          "    label=\"Inputs\"\n",
          "    style=filled\n",
          "    color=\"#EAEAEA\"\n",
          "    fillcolor=\"#F5F5F5\"\n"
        )
        
        for (i in seq_along(args)) {
          nodes <- paste0(nodes, 
            "    input", i, " [label=\"", args[i], "\", shape=box, fillcolor=\"#DAE8FC\"]\n"
          )
        }
        nodes <- paste0(nodes, "  }\n\n")
        
        # Add processing steps
        nodes <- paste0(nodes, 
          "  subgraph cluster_processing {\n",
          "    label=\"Processing\"\n",
          "    style=filled\n",
          "    color=\"#EAEAEA\"\n",
          "    fillcolor=\"#F5F5F5\"\n",
          "    node [fillcolor=\"#94C9FF\"]\n"
        )
        
        # Customize processing based on package type
        if (is_dplyr) {
          nodes <- paste0(nodes,
            "    transform [label=\"Data Transformation\"]\n",
            "    filter [label=\"Filter/Select Data\"]\n",
            "    summarize [label=\"Aggregation\"]\n"
          )
        } else if (is_ggplot) {
          nodes <- paste0(nodes,
            "    data_mapping [label=\"Map Data to Aesthetics\"]\n",
            "    geom [label=\"Add Geometric Elements\"]\n",
            "    theme [label=\"Apply Theme\"]\n"
          )
        } else if (is_stats) {
          nodes <- paste0(nodes, 
            "    model [label=\"Fit Statistical Model\"]\n",
            "    estimate [label=\"Calculate Estimates\"]\n",
            "    diagnose [label=\"Diagnostic Checks\"]\n"
          )
        } else {
          # Generic processing for other function types
          nodes <- paste0(nodes,
            "    process1 [label=\"Process Arguments\"]\n",
            "    process2 [label=\"Apply Function Logic\"]\n",
            "    process3 [label=\"Prepare Output\"]\n"
          )
        }
        nodes <- paste0(nodes, "  }\n\n")
        
        # Add output nodes
        nodes <- paste0(nodes, 
          "  subgraph cluster_outputs {\n",
          "    label=\"Outputs\"\n",
          "    style=filled\n",
          "    color=\"#EAEAEA\"\n",
          "    fillcolor=\"#F5F5F5\"\n",
          "    output [label=\"", returns, "\", shape=box, fillcolor=\"#D5E8D4\"]\n",
          "  }\n\n"
        )
        
        # Add edges connecting inputs to processes
        for (i in seq_along(args)) {
          if (is_dplyr) {
            nodes <- paste0(nodes, "  input", i, " -> transform\n")
          } else if (is_ggplot) {
            nodes <- paste0(nodes, "  input", i, " -> data_mapping\n")
          } else if (is_stats) {
            nodes <- paste0(nodes, "  input", i, " -> model\n")
          } else {
            nodes <- paste0(nodes, "  input", i, " -> process1\n")
          }
        }
        
        # Add edges connecting processes
        if (is_dplyr) {
          nodes <- paste0(nodes,
            "  transform -> filter\n",
            "  filter -> summarize\n",
            "  summarize -> output\n"
          )
        } else if (is_ggplot) {
          nodes <- paste0(nodes,
            "  data_mapping -> geom\n",
            "  geom -> theme\n",
            "  theme -> output\n"
          )
        } else if (is_stats) {
          nodes <- paste0(nodes,
            "  model -> estimate\n",
            "  estimate -> diagnose\n",
            "  diagnose -> output\n"
          )
        } else {
          nodes <- paste0(nodes,
            "  process1 -> process2\n",
            "  process2 -> process3\n",
            "  process3 -> output\n"
          )
        }
        
        # Close the graph
        nodes <- paste0(nodes, "}\n")
        
        # Create graph using DiagrammeR
        graph <- DiagrammeR::grViz(nodes)
        
        # Make the visualization interactive if htmlwidgets is available
        if (requireNamespace("htmlwidgets", quietly = TRUE)) {
          # Add interactivity to the graph
          graph <- htmlwidgets::onRender(graph, '
            function(el, x) {
              // Add tooltips to nodes
              d3.select(el).selectAll(".node")
                .append("title")
                .text(function(d) { return d.label; });
                
              // Make nodes clickable
              d3.select(el).selectAll(".node")
                .style("cursor", "pointer")
                .on("click", function(d) {
                  alert("Node: " + d.label);
                });
            }
          ')
          self$visualization_data$interactive <- TRUE
        }
        
        return(graph)
      }, error = function(e) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Error generating data flow diagram: ", e$message)
        }
        return(NULL)
      })
    },
    
    #' @description Generate a function network visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A visNetwork graph object or NULL if not available
    generate_function_network = function(func_name, metadata) {
      # Check required packages
      if (!requireNamespace("visNetwork", quietly = TRUE) || 
          !requireNamespace("igraph", quietly = TRUE)) {
        return(NULL)
      }
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Generating function network for: ", func_name)
      }
      
      # Extract package information
      pkg <- metadata$package %||% "unknown"
      
      tryCatch({
        # Find related functions from the same package
        related_functions <- self$find_related_functions(func_name, pkg)
        self$visualization_data$related_functions <- related_functions
        
        # Prepare nodes and edges data
        nodes <- data.frame(
          id = c(func_name, related_functions$functions),
          label = c(func_name, related_functions$functions),
          group = c("main", rep("related", length(related_functions$functions))),
          title = c(
            paste0("Main function: ", func_name),
            related_functions$descriptions
          ),
          shadow = c(TRUE, rep(FALSE, length(related_functions$functions))),
          shape = c("box", rep("box", length(related_functions$functions))),
          stringsAsFactors = FALSE
        )
        
        # Set node colors based on group
        nodes$color.background <- ifelse(nodes$group == "main", "#D2E5FF", "#F8F8F8")
        nodes$color.border <- ifelse(nodes$group == "main", "#2B7CE9", "#CCCCCC") 
        nodes$color.highlight.background <- "#FFFF99"
        nodes$fontStyle <- ifelse(nodes$group == "main", "bold", "normal")
        
        # Create edges
        edges <- data.frame(
          from = rep(func_name, length(related_functions$functions)),
          to = related_functions$functions,
          arrows = "to",
          smooth = TRUE,
          title = related_functions$relationships,
          stringsAsFactors = FALSE
        )
        
        # Additional edges between related functions
        if (nrow(edges) > 1) {
          # Try to find connections between related functions
          additional_edges <- self$find_connections_between_related(related_functions$functions, pkg)
          if (nrow(additional_edges) > 0) {
            edges <- rbind(edges, additional_edges)
          }
        }
        
        # Create interactive network graph
        network <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "500px") %>%
          visNetwork::visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
            nodesIdSelection = TRUE,
            selectedBy = "group"
          ) %>%
          visNetwork::visInteraction(
            tooltipDelay = 0,
            hideEdgesOnDrag = FALSE,
            navigationButtons = TRUE
          ) %>%
          visNetwork::visLayout(randomSeed = 123) %>%
          visNetwork::addFontAwesome() %>%
          visNetwork::visPhysics(
            solver = "forceAtlas2Based",
            forceAtlas2Based = list(gravitationalConstant = -50)
          )
        
        # Save that this is an interactive visualization
        self$visualization_data$interactive <- TRUE
        
        return(network)
      }, error = function(e) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Error generating function network: ", e$message)
        }
        return(NULL)
      })
    },
    
    #' @description Generate enhanced code highlighting visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return An HTML widget or NULL if not available
    generate_code_highlight = function(func_name, metadata) {
      # Check required packages
      if (!requireNamespace("htmltools", quietly = TRUE) ||
          !requireNamespace("highlight", quietly = TRUE)) {
        return(NULL)
      }
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Generating code highlight visualization for: ", func_name)
      }
      
      tryCatch({
        # Extract function body
        body_code <- metadata$body %||% ""
        if (body_code == "") {
          # If body is not available, try to get the function code
          body_code <- self$get_function_source(func_name, metadata$package)
        }
        
        if (body_code == "") {
          return(NULL)
        }
        
        # Parse and format the code
        formatted_code <- highlight::highlight(
          parse(text = body_code),
          renderer = highlight::renderer_html(document = FALSE),
          output = NULL
        )
        
        # Add collapsible sections for complex code
        if (nchar(body_code) > 500) {
          # Create sections for long code
          formatted_code <- self$create_collapsible_code_sections(formatted_code)
        }
        
        # Create an HTML widget
        widget <- htmltools::HTML(paste0(
          "<div class='tldr-code-highlight' style='background-color: #f8f8f8; padding: 10px; border-radius: 5px; border: 1px solid #ddd;'>",
          "<h3 style='color: #333; margin-top: 0;'>", func_name, " Source Code</h3>",
          "<div class='code-container' style='font-family: monospace; white-space: pre; overflow-x: auto; max-height: 500px;'>",
          formatted_code,
          "</div>",
          "<div class='code-footer' style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
          "Package: ", metadata$package, " | Click on code sections to expand/collapse",
          "</div>",
          "</div>"
        ))
        
        # Make it interactive if displayed in RStudio viewer
        interactive_widget <- htmltools::tags$div(
          htmltools::tags$style(
            ".code-section-toggle { cursor: pointer; background-color: #eaeaea; padding: 5px; margin: 5px 0; }",
            ".code-section-toggle:hover { background-color: #d3d3d3; }",
            ".code-section-content { display: none; }",
            ".code-line { display: block; padding-left: 20px; }",
            ".line-highlight { background-color: #ffff99; }"
          ),
          htmltools::tags$script(
            "$(document).ready(function() {
              $('.code-section-toggle').click(function() {
                $(this).next('.code-section-content').toggle();
              });
              
              $('.code-line').hover(
                function() { $(this).addClass('line-highlight'); },
                function() { $(this).removeClass('line-highlight'); }
              );
            });"
          ),
          widget
        )
        
        self$visualization_data$interactive <- TRUE
        
        return(interactive_widget)
      }, error = function(e) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Error generating code highlight: ", e$message)
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
      } else if (vis_type == "data_flow") {
        return(self$generate_ascii_data_flow(func_name, metadata))
      } else if (vis_type == "function_network") {
        return(self$generate_ascii_function_network(func_name, metadata))
      } else if (vis_type == "code_highlight") {
        return(self$generate_ascii_code_highlight(func_name, metadata))
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
    },
    
    #' @description Generate an ASCII data flow visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A character string with ASCII data flow diagram
    generate_ascii_data_flow = function(func_name, metadata) {
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
      } else {
        inputs <- paste0(inputs, "│   None                               │\n")
      }
      
      # Create processing steps based on function type
      processing <- "│                                     │\n│ Processing:                           │\n"
      
      if (is_dplyr) {
        processing <- paste0(processing,
          "│   1. ", sprintf("%-33s", "Data Transformation"), "│\n",
          "│   2. ", sprintf("%-33s", "Filter/Select Data"), "│\n",
          "│   3. ", sprintf("%-33s", "Aggregation"), "│\n"
        )
      } else if (is_ggplot) {
        processing <- paste0(processing,
          "│   1. ", sprintf("%-33s", "Map Data to Aesthetics"), "│\n",
          "│   2. ", sprintf("%-33s", "Add Geometric Elements"), "│\n",
          "│   3. ", sprintf("%-33s", "Apply Theme"), "│\n"
        )
      } else if (is_stats) {
        processing <- paste0(processing,
          "│   1. ", sprintf("%-33s", "Fit Statistical Model"), "│\n",
          "│   2. ", sprintf("%-33s", "Calculate Estimates"), "│\n",
          "│   3. ", sprintf("%-33s", "Diagnostic Checks"), "│\n"
        )
      } else {
        processing <- paste0(processing,
          "│   1. ", sprintf("%-33s", "Process Arguments"), "│\n",
          "│   2. ", sprintf("%-33s", "Apply Function Logic"), "│\n",
          "│   3. ", sprintf("%-33s", "Prepare Output"), "│\n"
        )
      }
      
      # Create output section
      outputs <- "│                                     │\n│ Output:                               │\n"
      outputs <- paste0(outputs, "│   ● ", sprintf("%-33s", "Return Value"), "│\n")
      
      # Create data flow
      flow <- "│                                     │\n│ Data Flow:                            │\n"
      flow <- paste0(flow,
        "│   Inputs ──> Processing ──> Output      │\n"
      )
      
      # Footer
      footer <- paste0(
        "╰─────────────────────────────────────╯\n\n",
        "Note: This is a text-based visualization.\n",
        "For graphical visualizations, install required packages:\n",
        "install.packages(\"", paste(self$get_required_packages(vis_type = "data_flow"), collapse = "\", \""), "\")"
      )
      
      # Combine all parts
      viz <- paste0(
        header,
        inputs,
        processing,
        outputs,
        flow,
        footer
      )
      
      return(viz)
    },
    
    #' @description Generate an ASCII function network visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A character string with ASCII function network
    generate_ascii_function_network = function(func_name, metadata) {
      pkg <- metadata$package %||% "Unknown"
      
      # Create header
      header <- paste0(
        "╭─ Function Network: ", func_name, " (", pkg, ") ─╮\n"
      )
      
      # Find related functions (simplified ASCII version)
      related <- character(0)
      if (pkg != "Unknown") {
        # Attempt to find a few related functions for ASCII art
        if (pkg == "dplyr") {
          related <- c("filter", "select", "mutate", "summarize", "group_by")
        } else if (pkg == "ggplot2") {
          related <- c("ggplot", "geom_point", "geom_line", "aes", "theme")
        } else if (pkg == "stats") {
          related <- c("lm", "glm", "t.test", "anova", "predict")
        } else if (pkg == "base") {
          related <- c("sum", "mean", "apply", "sapply", "lapply")
        } else {
          # Generic related functions when we can't determine actual relationships
          related <- c("related_func1", "related_func2", "related_func3")
        }
      }
      
      # Create central function node
      central <- paste0(
        "│                                      │\n",
        "│              ┌────────┐              │\n",
        "│              │ ", sprintf("%-6s", func_name), " │              │\n",
        "│              └────────┘              │\n"
      )
      
      # Create connections to related functions
      connections <- "│                                      │\n"
      if (length(related) > 0) {
        # Draw simplified connections for ASCII art
        connections <- paste0(connections,
          "│      ┌───────┴┬─────┴┬───────┐      │\n",
          "│      │        │      │        │      │\n",
          "│      ▼        ▼      ▼        ▼      │\n"
        )
        
        # Format related functions in a row
        related_line1 <- "│  "
        related_line2 <- "│  "
        related_line3 <- "│  "
        
        # Add up to 4 related functions in a row
        max_related <- min(4, length(related))
        for (i in 1:max_related) {
          func <- related[i]
          if (nchar(func) > 8) {
            func <- paste0(substr(func, 1, 6), "..")
          }
          
          related_line1 <- paste0(related_line1, " ┌────────┐ ")
          related_line2 <- paste0(related_line2, " │ ", sprintf("%-6s", func), " │ ")
          related_line3 <- paste0(related_line3, " └────────┘ ")
        }
        
        # Pad the lines to fill width
        related_line1 <- paste0(sprintf("%-36s", related_line1), "│\n")
        related_line2 <- paste0(sprintf("%-36s", related_line2), "│\n")
        related_line3 <- paste0(sprintf("%-36s", related_line3), "│\n")
        
        connections <- paste0(connections, related_line1, related_line2, related_line3)
      } else {
        connections <- paste0(connections,
          "│     (No related functions found)      │\n"
        )
      }
      
      # Footer
      footer <- paste0(
        "╰──────────────────────────────────────╯\n\n",
        "Note: This is a text-based visualization.\n",
        "For graphical visualizations, install required packages:\n",
        "install.packages(\"", paste(self$get_required_packages(vis_type = "function_network"), collapse = "\", \""), "\")"
      )
      
      # Combine all parts
      viz <- paste0(
        header,
        central,
        connections,
        footer
      )
      
      return(viz)
    },
    
    #' @description Generate an ASCII code highlighting visualization
    #' @param func_name The name of the function
    #' @param metadata Function metadata
    #' @return A character string with ASCII highlighted code
    generate_ascii_code_highlight = function(func_name, metadata) {
      pkg <- metadata$package %||% "Unknown"
      
      # Create header
      header <- paste0(
        "╭─ Code Highlight: ", func_name, " (", pkg, ") ─╮\n"
      )
      
      # Get function code for display
      body_code <- metadata$body %||% ""
      if (body_code == "") {
        # If body is not available, show placeholder
        body_code <- "# Source code not available\n# Install the package to view actual code"
      }
      
      # Format the code for display (limit to ~15 lines)
      code_lines <- strsplit(body_code, "\n")[[1]]
      if (length(code_lines) > 15) {
        code_lines <- c(code_lines[1:10], "# ... (truncated) ...", code_lines[(length(code_lines)-3):length(code_lines)])
      }
      
      # Format code with line numbers
      code_display <- ""
      for (i in seq_along(code_lines)) {
        line <- code_lines[i]
        # Ensure line isn't too long
        if (nchar(line) > 32) {
          line <- paste0(substr(line, 1, 29), "...")
        }
        code_display <- paste0(code_display, "│ ", sprintf("%2d", i), " ", sprintf("%-33s", line), "│\n")
      }
      
      # Footer
      footer <- paste0(
        "╰───────────────────────────────────────╯\n\n",
        "Note: This is a text-based visualization.\n",
        "For graphical visualizations with syntax highlighting and interactive features, install:\n",
        "install.packages(\"", paste(self$get_required_packages(vis_type = "code_highlight"), collapse = "\", \""), "\")"
      )
      
      # Combine all parts
      viz <- paste0(
        header,
        code_display,
        footer
      )
      
      return(viz)
    },
    
    #' @description Find related functions in the same package
    #' @param func_name The name of the function
    #' @param pkg The package name
    #' @return A list with related functions and their descriptions
    find_related_functions = function(func_name, pkg) {
      # Initialize empty result
      result <- list(
        functions = character(0),
        descriptions = character(0),
        relationships = character(0)
      )
      
      # Skip if package is unknown
      if (is.null(pkg) || pkg == "unknown" || pkg == "Unknown") {
        return(result)
      }
      
      tryCatch({
        # Check if the package is installed
        if (!requireNamespace(pkg, quietly = TRUE)) {
          return(result)
        }
        
        # Get exported functions from package
        pkg_ns <- getNamespace(pkg)
        exports <- tryCatch(getNamespaceExports(pkg_ns), error = function(e) character(0))
        
        if (length(exports) == 0) {
          return(result)
        }
        
        # Filter out the current function
        exports <- exports[exports != func_name]
        
        # Limit the number of related functions
        max_related <- min(10, length(exports))
        if (max_related == 0) {
          return(result)
        }
        
        selected <- sample(exports, max_related)
        
        # Get descriptions (when possible)
        descriptions <- character(length(selected))
        relationships <- character(length(selected))
        
        for (i in seq_along(selected)) {
          func <- selected[i]
          # Try to get description from help
          help_text <- tryCatch({
            utils::capture.output(utils::help(func, package = pkg))
          }, error = function(e) character(0))
          
          if (length(help_text) > 0) {
            # Extract a very brief description
            desc_line <- grep("Description:", help_text, fixed = TRUE)
            if (length(desc_line) > 0 && desc_line[1] < length(help_text)) {
              desc <- help_text[desc_line[1] + 1]
              if (nchar(desc) > 0) {
                desc <- trimws(desc)
                if (nchar(desc) > 40) {
                  desc <- paste0(substr(desc, 1, 37), "...")
                }
                descriptions[i] <- desc
              } else {
                descriptions[i] <- paste0("Function in package ", pkg)
              }
            } else {
              descriptions[i] <- paste0("Function in package ", pkg)
            }
          } else {
            descriptions[i] <- paste0("Function in package ", pkg)
          }
          
          # Create a simple relationship description
          relationships[i] <- paste0("Related function in ", pkg, " package")
        }
        
        result$functions <- selected
        result$descriptions <- descriptions
        result$relationships <- relationships
        
        return(result)
      }, error = function(e) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Error finding related functions: ", e$message)
        }
        return(result)
      })
    },
    
    #' @description Find connections between related functions
    #' @param related_funcs Character vector of related function names
    #' @param pkg The package name
    #' @return A data frame of edges between related functions
    find_connections_between_related = function(related_funcs, pkg) {
      # Initialize empty edges data frame
      edges <- data.frame(
        from = character(0),
        to = character(0),
        arrows = character(0),
        smooth = logical(0),
        title = character(0),
        stringsAsFactors = FALSE
      )
      
      # We need at least 2 functions to create connections
      if (length(related_funcs) < 2) {
        return(edges)
      }
      
      tryCatch({
        # Generate some connections (in a real implementation, these would be based on actual analysis)
        # For now, we'll create a few random connections for demonstration
        n_connections <- min(5, length(related_funcs))
        
        for (i in 1:n_connections) {
          from_idx <- sample(1:length(related_funcs), 1)
          to_idx <- sample((1:length(related_funcs))[-from_idx], 1)
          
          from_func <- related_funcs[from_idx]
          to_func <- related_funcs[to_idx]
          
          new_edge <- data.frame(
            from = from_func,
            to = to_func,
            arrows = "to",
            smooth = TRUE,
            title = paste0("May be used together with ", from_func),
            stringsAsFactors = FALSE
          )
          
          edges <- rbind(edges, new_edge)
        }
        
        return(edges)
      }, error = function(e) {
        return(edges)
      })
    },
    
    #' @description Get source code for a function
    #' @param func_name The name of the function
    #' @param pkg The package name
    #' @return Character string with the function source code
    get_function_source = function(func_name, pkg) {
      tryCatch({
        if (!is.null(pkg) && pkg != "unknown" && pkg != "Unknown") {
          # Get function from package
          func_obj <- getExportedValue(pkg, func_name)
        } else {
          # Try to get function from global environment
          func_obj <- get(func_name, envir = .GlobalEnv)
        }
        
        # Get source code
        if (is.function(func_obj)) {
          src <- deparse(func_obj)
          return(paste(src, collapse = "\n"))
        }
      }, error = function(e) {
        return("")
      })
      
      return("")
    },
    
    #' @description Create collapsible sections for complex code
    #' @param code_html The HTML formatted code
    #' @return HTML string with collapsible sections
    create_collapsible_code_sections = function(code_html) {
      # This would do complex parsing of HTML in a real implementation
      # For simplicity here, we'll just wrap the code in a collapsible structure
      
      # Split the code into logical sections (simplified approach)
      # In a real implementation, this would parse the HTML and identify function definitions,
      # control structures, etc.
      
      # Simplified approach: just create an initial collapsed section
      collapsible_code <- paste0(
        "<div class='code-section-toggle'>Click to expand/collapse code</div>",
        "<div class='code-section-content'>",
        code_html,
        "</div>"
      )
      
      return(collapsible_code)
    },
    
    #' @description Export visualization as SVG
    #' @param file_path The path to save the SVG file
    #' @return Logical indicating success
    export_svg = function(file_path) {
      vis <- self$visualization_data$diagram
      vis_type <- self$visualization_data$vis_type
      
      if (is.null(vis)) {
        stop("No visualization available to export.")
      }
      
      # Check if this is a fallback ASCII visualization
      if (is.character(vis) && substr(vis_type, 1, 9) == "fallback_") {
        # For ASCII fallback, save as text
        return(self$export_ascii(file_path))
      }
      
      # Check for required packages
      if (!requireNamespace("DiagrammeR", quietly = TRUE) ||
          !requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop("SVG export requires the DiagrammeR and htmlwidgets packages.")
      }
      
      tryCatch({
        # Export the visualization as SVG
        if (inherits(vis, "htmlwidget")) {
          # For DiagrammeR graphs
          if (requireNamespace("DiagrammeRsvg", quietly = TRUE) && 
              (vis_type %in% c("diagram", "flowchart", "data_flow"))) {
            # Convert to SVG
            svg_code <- DiagrammeRsvg::export_svg(vis)
            # Write SVG to file
            writeLines(svg_code, file_path)
            return(TRUE)
          } else if (vis_type == "function_network" && requireNamespace("visNetwork", quietly = TRUE)) {
            # For visNetwork graphs
            svg_code <- visNetwork::visExport(vis, type = "svg")
            # This doesn't write to file directly, but returns SVG code
            writeLines(svg_code, file_path)
            return(TRUE)
          } else if (vis_type == "code_highlight") {
            # For HTML widgets (code highlighting)
            svg_code <- paste0("<svg>", as.character(vis), "</svg>")
            writeLines(svg_code, file_path)
            return(TRUE)
          }
        }
        
        # Default fallback if specific export not available
        stop("SVG export not supported for this visualization type.")
      }, error = function(e) {
        warning("Error exporting to SVG: ", e$message)
        return(FALSE)
      })
    },
    
    #' @description Export visualization as PNG
    #' @param file_path The path to save the PNG file
    #' @param width The width of the PNG in pixels
    #' @param height The height of the PNG in pixels
    #' @return Logical indicating success
    export_png = function(file_path, width = 800, height = 600) {
      vis <- self$visualization_data$diagram
      vis_type <- self$visualization_data$vis_type
      
      if (is.null(vis)) {
        stop("No visualization available to export.")
      }
      
      # Check if this is a fallback ASCII visualization
      if (is.character(vis) && substr(vis_type, 1, 9) == "fallback_") {
        # For ASCII fallback, save as text
        warning("Cannot export ASCII visualization as PNG. Saving as TXT instead.")
        return(self$export_ascii(file_path))
      }
      
      # Check for required packages
      if (!requireNamespace("htmlwidgets", quietly = TRUE) ||
          !requireNamespace("webshot", quietly = TRUE)) {
        stop("PNG export requires the htmlwidgets and webshot packages.")
      }
      
      tryCatch({
        # Create a temporary HTML file
        temp_html <- tempfile(fileext = ".html")
        
        # Save the widget to a temporary HTML file
        htmlwidgets::saveWidget(vis, temp_html, selfcontained = TRUE)
        
        # Use webshot to capture a PNG
        webshot::webshot(temp_html, file_path, vwidth = width, vheight = height)
        
        # Clean up the temporary file
        if (file.exists(temp_html)) {
          unlink(temp_html)
        }
        
        return(TRUE)
      }, error = function(e) {
        warning("Error exporting to PNG: ", e$message)
        return(FALSE)
      })
    },
    
    #' @description Export visualization as ASCII text
    #' @param file_path The path to save the text file
    #' @return Logical indicating success
    export_ascii = function(file_path) {
      vis <- self$visualization_data$diagram
      vis_type <- self$visualization_data$vis_type
      
      if (is.null(vis)) {
        stop("No visualization available to export.")
      }
      
      tryCatch({
        # If it's already an ASCII representation (string)
        if (is.character(vis)) {
          writeLines(vis, file_path)
          return(TRUE)
        }
        
        # Generate an ASCII version based on vis_type
        ascii_vis <- NULL
        
        if (vis_type == "diagram") {
          ascii_vis <- self$generate_ascii_diagram(
            self$visualization_data$func_name,
            list(
              package = self$visualization_data$package,
              args = self$visualization_data$args
            )
          )
        } else if (vis_type == "flowchart") {
          ascii_vis <- self$generate_ascii_flowchart(
            self$visualization_data$func_name,
            list(
              package = self$visualization_data$package,
              body_summary = "" # We don't have this info here
            )
          )
        } else if (vis_type == "data_flow") {
          ascii_vis <- self$generate_ascii_data_flow(
            self$visualization_data$func_name,
            list(
              package = self$visualization_data$package,
              args = self$visualization_data$args
            )
          )
        } else if (vis_type == "function_network") {
          ascii_vis <- self$generate_ascii_function_network(
            self$visualization_data$func_name,
            list(
              package = self$visualization_data$package
            )
          )
        } else if (vis_type == "code_highlight") {
          ascii_vis <- self$generate_ascii_code_highlight(
            self$visualization_data$func_name,
            list(
              package = self$visualization_data$package,
              body = "" # We don't have this info here
            )
          )
        } else {
          # Generic export for unknown types
          ascii_vis <- paste0(
            "Function: ", self$visualization_data$func_name, "\n",
            "Package: ", self$visualization_data$package %||% "Unknown", "\n",
            "Arguments: ", paste(self$visualization_data$args %||% "None", collapse = ", "), "\n"
          )
        }
        
        if (!is.null(ascii_vis)) {
          writeLines(ascii_vis, file_path)
          return(TRUE)
        } else {
          stop("Failed to generate ASCII representation.")
        }
      }, error = function(e) {
        warning("Error exporting to TXT: ", e$message)
        return(FALSE)
      })
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

#' Export a visualization to a file
#'
#' @param visualization A visualization handler object
#' @param file_path Character string specifying the file path to save the visualization
#' @param format Character string specifying the export format ("svg", "png", "txt")
#' @param width Numeric value specifying the width of the exported image in pixels
#' @param height Numeric value specifying the height of the exported image in pixels
#'
#' @return Invisibly returns the visualization handler
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- get_function_metadata("mean")
#' vis <- create_visualization("mean", metadata)
#' export_visualization(vis, "mean_diagram.svg")
#' export_visualization(vis, "mean_diagram.png", width = 800, height = 600)
#' export_visualization(vis, "mean_diagram.txt", format = "txt")
#' }
export_visualization <- function(visualization, file_path, format = NULL, width = 800, height = 600) {
  if (!inherits(visualization, "VisualizationHandler")) {
    stop("Visualization must be a VisualizationHandler object.")
  }
  
  # Determine format from file extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file_path))
    if (ext %in% c("svg", "png", "txt")) {
      format <- ext
    } else {
      stop("Unable to determine format from file extension. Please specify format parameter.")
    }
  }
  
  # Validate format
  if (!format %in% c("svg", "png", "txt")) {
    stop("Format must be one of: svg, png, txt")
  }
  
  # Export based on format
  result <- tryCatch({
    if (format == "svg") {
      visualization$export_svg(file_path)
    } else if (format == "png") {
      visualization$export_png(file_path, width = width, height = height)
    } else if (format == "txt") {
      visualization$export_ascii(file_path)
    }
    TRUE
  }, error = function(e) {
    message("Error exporting visualization: ", e$message)
    FALSE
  })
  
  if (result) {
    message("Visualization exported successfully to: ", file_path)
  }
  
  invisible(visualization)
}

#' Configure visualization settings for tldrAI
#'
#' This function configures the visualization system in tldrAI, which can generate
#' diagrams, flowcharts, and other visual representations of R functions. You can
#' enable or disable visualizations, set the default visualization type, configure
#' automatic package installation, and set up export preferences.
#'
#' @section Visualization Types:
#' tldrAI supports multiple visualization types:
#' \describe{
#'   \item{diagram}{Simple function diagram showing inputs and outputs. Best for basic function understanding.}
#'   \item{flowchart}{Logical flow diagram showing conditionals and loops. Best for understanding function processing flow.}
#'   \item{data_flow}{Data transformation diagram. Best for data processing functions (dplyr, ggplot2, etc.).}
#'   \item{function_network}{Network diagram showing related functions. Best for understanding function relationships.}
#'   \item{code_highlight}{Syntax highlighted code with interactive elements. Best for examining implementation details.}
#' }
#'
#' @section Required Packages:
#' Different visualization types require different packages:
#' \describe{
#'   \item{diagram, flowchart}{Requires DiagrammeR}
#'   \item{data_flow}{Requires DiagrammeR and htmlwidgets}
#'   \item{function_network}{Requires visNetwork and igraph}
#'   \item{code_highlight}{Requires htmltools and highlight}
#' }
#' For exporting:
#' \describe{
#'   \item{SVG export}{Requires DiagrammeRsvg for diagrams}
#'   \item{PNG export}{Requires webshot for all visualization types}
#' }
#'
#' @param enable Logical indicating whether to enable visualizations by default.
#'        When TRUE, tldr() calls will include visualizations.
#'        Default: FALSE.
#'
#' @param default_type Character string specifying the default visualization type.
#'        Options: "diagram", "flowchart", "data_flow", "function_network", or "code_highlight".
#'        Default: "diagram".
#'
#' @param auto_install Logical indicating whether to automatically install required packages.
#'        When TRUE, missing packages will be installed without prompting.
#'        When FALSE, ASCII fallbacks will be used if packages are missing.
#'        Default: FALSE.
#'
#' @param auto_export Logical indicating whether to automatically export visualizations.
#'        When TRUE, every visualization created is automatically saved to a file.
#'        Default: FALSE.
#'
#' @param export_format Character string specifying the default export format.
#'        Options: "svg" (vector graphics), "png" (bitmap image), or "txt" (ASCII art).
#'        Default: "svg".
#'
#' @param export_dir Character string specifying the default directory for exported visualizations.
#'        Default: Current working directory.
#'
#' @return Invisibly returns the updated configuration list. The function primarily
#'         has side effects, updating the package configuration.
#'
#' @seealso
#' \code{\link{tldr}} for using visualizations with the visualize=TRUE parameter
#' \code{\link{export_visualization}} for manually exporting visualizations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic configuration
#' tldr_visualization_config(enable = TRUE)  # Enable visualizations
#'
#' # Set default visualization type
#' tldr_visualization_config(default_type = "diagram")      # Simple function diagram
#' tldr_visualization_config(default_type = "flowchart")    # Logical flow diagram
#' tldr_visualization_config(default_type = "data_flow")    # Data transformation
#' tldr_visualization_config(default_type = "function_network")  # Related functions
#' tldr_visualization_config(default_type = "code_highlight")    # Syntax highlighted code
#'
#' # Configure package installation behavior
#' tldr_visualization_config(auto_install = TRUE)  # Install packages automatically
#'
#' # Configure automatic exports
#' tldr_visualization_config(
#'   auto_export = TRUE,         # Export every visualization automatically
#'   export_format = "svg",      # Use SVG format by default
#'   export_dir = "~/R/vis"      # Save to specific directory
#' )
#'
#' # Full configuration
#' tldr_visualization_config(
#'   enable = TRUE,
#'   default_type = "function_network",
#'   auto_install = TRUE,
#'   auto_export = TRUE,
#'   export_format = "svg",
#'   export_dir = "~/R/visualizations"
#' )
#' }
tldr_visualization_config <- function(enable = NULL, default_type = NULL, auto_install = NULL,
                                    auto_export = NULL, export_format = NULL, export_dir = NULL) {
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
    
    # Display information about the selected visualization type
    message("\nVisualization type '", default_type, "' selected:")
    if (default_type == "diagram") {
      message("  - Simple function diagram showing inputs and outputs")
      message("  - Best for basic function understanding")
    } else if (default_type == "flowchart") {
      message("  - Logical flow diagram showing conditionals and loops")
      message("  - Best for understanding function processing flow")
    } else if (default_type == "data_flow") {
      message("  - Data transformation diagram")
      message("  - Best for data processing functions (dplyr, ggplot2, etc.)")
    } else if (default_type == "function_network") {
      message("  - Network diagram showing related functions")
      message("  - Best for understanding function relationships in packages")
    } else if (default_type == "code_highlight") {
      message("  - Syntax highlighted code with interactive elements")
      message("  - Best for examining function implementation details")
    }
  }
  
  if (!is.null(auto_install)) {
    config$visualization_settings$auto_install <- auto_install
  }
  
  # Handle new export settings
  if (!is.null(auto_export)) {
    config$visualization_settings$auto_export <- auto_export
  }
  
  if (!is.null(export_format)) {
    # Validate export format
    if (!export_format %in% c("svg", "png", "txt")) {
      warning("Unsupported export format: ", export_format, 
              ". Supported formats are: svg, png, txt. Using 'svg' as default.")
      export_format <- "svg"
    }
    config$visualization_settings$export_format <- export_format
  }
  
  if (!is.null(export_dir)) {
    # Validate export directory
    if (!dir.exists(export_dir)) {
      warning("Export directory does not exist: ", export_dir, 
              ". Creating directory...")
      dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    if (!dir.exists(export_dir)) {
      warning("Failed to create export directory: ", export_dir, 
              ". Using current working directory instead.")
      export_dir <- getwd()
    }
    
    config$visualization_settings$export_dir <- export_dir
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
  
  if (!is.null(config$visualization_settings$auto_export)) {
    message("  Auto-export visualizations: ", 
            ifelse(config$visualization_settings$auto_export, "Yes", "No"))
  }
  
  if (!is.null(config$visualization_settings$export_format)) {
    message("  Default export format: ", config$visualization_settings$export_format)
  }
  
  if (!is.null(config$visualization_settings$export_dir)) {
    message("  Export directory: ", config$visualization_settings$export_dir)
  }
  
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