#' Script visualization handler for tldrAI
#'
#' @description Add-on to the VisualizationHandler class to support script visualization.
#' These methods are integrated with the main VisualizationHandler class.
#'
#' @importFrom R6 R6Class
#' @keywords internal
VisualizationHandlerScriptExtension <- list(
  #' @description Generate a script dependency visualization
  #' @param files Character vector of file paths to R scripts
  #' @return A visualization object
  generate_script_dependency_visualization = function(files) {
    if (!requireNamespace("visNetwork", quietly = TRUE) || 
        !requireNamespace("igraph", quietly = TRUE)) {
      return(self$generate_ascii_script_dependency(files))
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Generating script dependency visualization")
    }
    
    # Extract script details
    script_data <- self$extract_script_metadata(files)
    
    tryCatch({
      # Create nodes for scripts and their dependencies
      nodes <- data.frame(
        id = character(0),
        label = character(0),
        group = character(0),
        title = character(0),
        shape = character(0),
        stringsAsFactors = FALSE
      )
      
      edges <- data.frame(
        from = character(0),
        to = character(0),
        arrows = character(0),
        title = character(0),
        stringsAsFactors = FALSE
      )
      
      # Add script nodes
      for (file in files) {
        file_basename <- basename(file)
        nodes <- rbind(nodes, data.frame(
          id = file,
          label = file_basename,
          group = "script",
          title = file,
          shape = "square",
          stringsAsFactors = FALSE
        ))
      }
      
      # Add package nodes and dependencies
      all_packages <- character(0)
      for (file in files) {
        # Extract packages from script data
        packages <- script_data[[file]]$packages
        if (length(packages) > 0) {
          all_packages <- union(all_packages, packages)
          
          # Add edges between script and packages
          for (pkg in packages) {
            edges <- rbind(edges, data.frame(
              from = file,
              to = pkg,
              arrows = "to",
              title = paste("Uses package:", pkg),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Add package nodes
      for (pkg in all_packages) {
        nodes <- rbind(nodes, data.frame(
          id = pkg,
          label = pkg,
          group = "package",
          title = paste("Package:", pkg),
          shape = "circle",
          stringsAsFactors = FALSE
        ))
      }
      
      # Add function dependencies between scripts
      for (file1 in files) {
        functions_defined <- script_data[[file1]]$functions_defined
        if (length(functions_defined) > 0) {
          for (file2 in files) {
            if (file1 != file2) {
              functions_called <- script_data[[file2]]$functions_called
              common_functions <- intersect(functions_defined, functions_called)
              if (length(common_functions) > 0) {
                edges <- rbind(edges, data.frame(
                  from = file1,
                  to = file2,
                  arrows = "to",
                  title = paste("Uses functions:", paste(common_functions, collapse=", ")),
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
        }
      }
      
      # Create the network visualization
      network <- visNetwork::visNetwork(nodes, edges) %>%
        visNetwork::visGroups(groupname = "script", color = "#3498db") %>%
        visNetwork::visGroups(groupname = "package", color = "#2ecc71") %>%
        visNetwork::visLayout(randomSeed = 123) %>%
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      
      return(network)
    }, error = function(e) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Error generating script dependency visualization: ", e$message)
      }
      return(self$generate_ascii_script_dependency(files))
    })
  },
  
  #' @description Generate a script flow visualization
  #' @param files Character vector of file paths to R scripts
  #' @return A visualization object
  generate_script_flow_visualization = function(files) {
    if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
      return(self$generate_ascii_script_flow(files))
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Generating script flow visualization")
    }
    
    # For flow visualization, focus on the first file if multiple files are provided
    file <- files[1]
    
    # Extract script details
    script_content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    blocks <- self$extract_code_blocks(script_content)
    
    tryCatch({
      # Create dot language for the flow graph
      dot_code <- paste0(
        "digraph script_flow {\n",
        "  graph [rankdir=TB, fontname=Arial, fontsize=12, dpi=300, bgcolor=\"#FFFFFF\"]\n",
        "  node [fontname=Arial, fontsize=10, shape=box, style=filled, fillcolor=\"#E6F3FF\", margin=0.2]\n",
        "  edge [fontname=Arial, fontsize=9, color=\"#333333\"]\n",
        
        # Add header with script name
        "  label=\"Script Flow: ", basename(file), "\"\n",
        "  labelloc=\"t\"\n",
        "  fontsize=14\n",
        "  fontname=\"Arial-Bold\"\n\n"
      )
      
      # Add nodes for each code block
      for (i in seq_along(blocks)) {
        # Determine block type and color based on content
        block_type <- "code"
        fill_color <- "#F5F5F5"
        
        if (grepl("read\\.|import|load|fread|scan", blocks[i], ignore.case = TRUE)) {
          block_type <- "data_import"
          fill_color <- "#E1F5FE"
        } else if (grepl("plot|ggplot|chart|histogram|boxplot", blocks[i], ignore.case = TRUE)) {
          block_type <- "visualization"
          fill_color <- "#FFF9C4"
        } else if (grepl("transform|mutate|select|filter|summarize|group_by", blocks[i], ignore.case = TRUE)) {
          block_type <- "data_processing"
          fill_color <- "#E8F5E9"
        } else if (grepl("model|lm\\(|glm\\(|train|fit", blocks[i], ignore.case = TRUE)) {
          block_type <- "model_training"
          fill_color <- "#F3E5F5"
        } else if (grepl("write|save|export|print", blocks[i], ignore.case = TRUE)) {
          block_type <- "output"
          fill_color <- "#FFEBEE"
        }
        
        # Create block summary (first line or truncated content)
        first_line <- strsplit(blocks[i], "\n")[[1]][1]
        if (nchar(first_line) > 40) {
          first_line <- paste0(substr(first_line, 1, 37), "...")
        }
        # Escape special characters for dot
        first_line <- gsub("\"", "\\\\\"", first_line)
        
        # Add node for this block
        dot_code <- paste0(dot_code,
                          "  block", i, " [label=\"", first_line, "\", fillcolor=\"", fill_color, "\"]\n")
        
        # Add edge to previous block
        if (i > 1) {
          dot_code <- paste0(dot_code, "  block", i-1, " -> block", i, "\n")
        }
      }
      
      # Close the graph
      dot_code <- paste0(dot_code, "}\n")
      
      # Create graph using DiagrammeR
      graph <- DiagrammeR::grViz(dot_code)
      return(graph)
    }, error = function(e) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Error generating script flow visualization: ", e$message)
      }
      return(self$generate_ascii_script_flow(files))
    })
  },
  
  #' @description Generate a script package visualization
  #' @param files Character vector of file paths to R scripts
  #' @return A visualization object
  generate_script_package_visualization = function(files) {
    if (!requireNamespace("visNetwork", quietly = TRUE)) {
      return(self$generate_ascii_script_package(files))
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Generating script package visualization")
    }
    
    # Extract script details
    script_data <- self$extract_script_metadata(files)
    
    tryCatch({
      # Group packages by category
      pkg_categories <- list(
        "Data Manipulation" = c("dplyr", "tidyr", "purrr", "plyr", "data.table"),
        "Visualization" = c("ggplot2", "plotly", "lattice", "ggvis", "highcharter"),
        "Machine Learning" = c("caret", "randomForest", "xgboost", "e1071", "nnet", "rpart"),
        "Statistics" = c("stats", "lme4", "nlme", "survival", "MASS", "car"),
        "I/O" = c("readr", "readxl", "haven", "jsonlite", "xml2", "httr")
      )
      
      # Prepare nodes dataframe
      nodes <- data.frame(
        id = character(0),
        label = character(0),
        group = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      )
      
      # Add script nodes
      for (file in files) {
        nodes <- rbind(nodes, data.frame(
          id = file,
          label = basename(file),
          group = "Script",
          value = 30,
          stringsAsFactors = FALSE
        ))
      }
      
      # Collect all packages
      all_packages <- character(0)
      for (file in files) {
        packages <- script_data[[file]]$packages
        all_packages <- union(all_packages, packages)
      }
      
      # Add package nodes
      for (pkg in all_packages) {
        # Determine package category
        category <- "Other"
        for (cat_name in names(pkg_categories)) {
          if (pkg %in% pkg_categories[[cat_name]]) {
            category <- cat_name
            break
          }
        }
        
        nodes <- rbind(nodes, data.frame(
          id = pkg,
          label = pkg,
          group = category,
          value = 15,
          stringsAsFactors = FALSE
        ))
      }
      
      # Create edges dataframe
      edges <- data.frame(
        from = character(0),
        to = character(0),
        width = numeric(0),
        arrows = character(0),
        stringsAsFactors = FALSE
      )
      
      # Add edges between scripts and packages
      for (file in files) {
        packages <- script_data[[file]]$packages
        if (length(packages) > 0) {
          for (pkg in packages) {
            edges <- rbind(edges, data.frame(
              from = file,
              to = pkg,
              width = 2,
              arrows = "from",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Create visNetwork
      network <- visNetwork::visNetwork(nodes, edges) %>%
        visNetwork::visNodes(font = list(size = 12)) %>%
        visNetwork::visLayout(randomSeed = 123) %>%
        visNetwork::visLegend() %>%
        visNetwork::visOptions(highlightNearest = TRUE)
      
      return(network)
    }, error = function(e) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Error generating script package visualization: ", e$message)
      }
      return(self$generate_ascii_script_package(files))
    })
  },
  
  #' @description Generate a script structure visualization
  #' @param files Character vector of file paths to R scripts
  #' @return A visualization object
  generate_script_structure_visualization = function(files) {
    if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
      return(self$generate_ascii_script_structure(files))
    }
    
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Generating script structure visualization")
    }
    
    # For structure visualization, focus on the first file if multiple files are provided
    file <- files[1]
    
    # Extract script details
    script_content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    functions <- self$extract_functions_defined(script_content)
    variables <- self$extract_variables(script_content)
    
    tryCatch({
      # Create dot language for the structure diagram
      dot_code <- paste0(
        "digraph script_structure {\n",
        "  graph [rankdir=TB, fontname=Arial, fontsize=12, dpi=300, bgcolor=\"#FFFFFF\"]\n",
        "  node [fontname=Arial, fontsize=10, shape=box, style=filled, margin=0.2]\n",
        "  edge [fontname=Arial, fontsize=9, color=\"#333333\"]\n",
        
        # Add header with script name
        "  label=\"Script Structure: ", basename(file), "\"\n",
        "  labelloc=\"t\"\n",
        "  fontsize=14\n",
        "  fontname=\"Arial-Bold\"\n\n",
        
        # Add script node
        "  script [label=\"", basename(file), "\", shape=box3d, fillcolor=\"#B3E5FC\"]\n\n"
      )
      
      # Add subgraph for functions
      dot_code <- paste0(dot_code, 
                        "  subgraph cluster_functions {\n",
                        "    label=\"Functions\"\n",
                        "    style=filled\n",
                        "    color=\"#EAEAEA\"\n",
                        "    fillcolor=\"#F5F5F5\"\n"
      )
      
      # Add function nodes
      if (length(functions) > 0) {
        for (i in seq_along(functions)) {
          func_name <- functions[i]
          # Escape special characters for dot
          func_name <- gsub("\"", "\\\\\"", func_name)
          
          dot_code <- paste0(dot_code,
                            "    func", i, " [label=\"", func_name, "()\", fillcolor=\"#E1F5FE\"]\n")
          dot_code <- paste0(dot_code, "    script -> func", i, "\n")
        }
      } else {
        dot_code <- paste0(dot_code, "    no_funcs [label=\"No functions defined\", fillcolor=\"#E1F5FE\"]\n")
      }
      dot_code <- paste0(dot_code, "  }\n\n")
      
      # Add subgraph for variables
      dot_code <- paste0(dot_code, 
                        "  subgraph cluster_variables {\n",
                        "    label=\"Key Variables\"\n",
                        "    style=filled\n",
                        "    color=\"#EAEAEA\"\n",
                        "    fillcolor=\"#F5F5F5\"\n"
      )
      
      # Add variable nodes
      if (length(variables) > 0) {
        for (i in seq_along(variables)) {
          var_name <- variables[i]
          
          # Try to determine variable type from the script content
          var_type <- "unknown"
          if (grepl(paste0(var_name, "\\s*<-\\s*data\\.frame"), script_content)) {
            var_type <- "data.frame"
          } else if (grepl(paste0(var_name, "\\s*<-\\s*list"), script_content)) {
            var_type <- "list"
          } else if (grepl(paste0(var_name, "\\s*<-\\s*c\\("), script_content)) {
            var_type <- "vector"
          } else if (grepl(paste0(var_name, "\\s*<-\\s*\""), script_content) || 
                    grepl(paste0(var_name, "\\s*<-\\s*'"), script_content)) {
            var_type <- "character"
          } else if (grepl(paste0(var_name, "\\s*<-\\s*\\d"), script_content)) {
            var_type <- "numeric"
          } else if (grepl(paste0(var_name, "\\s*<-\\s*TRUE|", var_name, "\\s*<-\\s*FALSE"), script_content)) {
            var_type <- "logical"
          }
          
          # Escape special characters for dot
          var_name <- gsub("\"", "\\\\\"", var_name)
          
          dot_code <- paste0(dot_code,
                            "    var", i, " [label=\"", var_name, " (", var_type, ")\", fillcolor=\"#F1F8E9\"]\n")
          dot_code <- paste0(dot_code, "    script -> var", i, "\n")
        }
      } else {
        dot_code <- paste0(dot_code, "    no_vars [label=\"No key variables identified\", fillcolor=\"#F1F8E9\"]\n")
      }
      dot_code <- paste0(dot_code, "  }\n\n")
      
      # Close the graph
      dot_code <- paste0(dot_code, "}\n")
      
      # Create graph using DiagrammeR
      graph <- DiagrammeR::grViz(dot_code)
      return(graph)
    }, error = function(e) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Error generating script structure visualization: ", e$message)
      }
      return(self$generate_ascii_script_structure(files))
    })
  },
  
  #' @description Generate an ASCII script dependency diagram
  #' @param files Character vector of file paths to R scripts
  #' @return A character string with ASCII visualization
  generate_ascii_script_dependency = function(files) {
    # Extract script details
    script_data <- self$extract_script_metadata(files)
    
    # Create header
    header <- "╭────── Script Dependencies ───────╮\n"
    
    # Create dependencies section
    deps_section <- ""
    
    for (file in files) {
      deps_section <- paste0(deps_section, "│ ", sprintf("%-33s", basename(file)), " │\n")
      
      # Add packages
      packages <- script_data[[file]]$packages
      if (length(packages) > 0) {
        deps_section <- paste0(deps_section, "│ Packages:                           │\n")
        for (i in 1:min(5, length(packages))) {
          deps_section <- paste0(deps_section, "│   ● ", sprintf("%-31s", packages[i]), " │\n")
        }
        if (length(packages) > 5) {
          deps_section <- paste0(deps_section, "│   ● ", sprintf("%-31s", paste0("... and ", length(packages)-5, " more")), " │\n")
        }
      } else {
        deps_section <- paste0(deps_section, "│ No packages detected                │\n")
      }
      
      # Add separator between files
      if (file != files[length(files)]) {
        deps_section <- paste0(deps_section, "│                                     │\n")
      }
    }
    
    # Footer
    footer <- paste0(
      "╰─────────────────────────────────╯\n\n",
      "Note: This is a text-based visualization.\n",
      "For graphical visualizations, install visNetwork: install.packages(\"visNetwork\")"
    )
    
    # Combine all parts
    viz <- paste0(
      header,
      deps_section,
      footer
    )
    
    return(viz)
  },
  
  #' @description Generate an ASCII script flow diagram
  #' @param files Character vector of file paths to R scripts
  #' @return A character string with ASCII visualization
  generate_ascii_script_flow = function(files) {
    # For flow visualization, focus on the first file if multiple files are provided
    file <- files[1]
    
    # Extract script content
    script_content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    blocks <- self$extract_code_blocks(script_content)
    
    # Create header
    header <- paste0(
      "╭─ Script Flow: ", basename(file), " ─╮\n"
    )
    
    # Create flow section
    flow_section <- ""
    if (length(blocks) > 0) {
      for (i in seq_along(blocks)) {
        # Get first line of the block as summary
        first_line <- strsplit(blocks[i], "\n")[[1]][1]
        if (nchar(first_line) > 30) {
          first_line <- paste0(substr(first_line, 1, 27), "...")
        }
        
        if (i == 1) {
          flow_section <- paste0(flow_section, "│ ┌─", sprintf("%-36s", first_line), "│\n")
        } else {
          flow_section <- paste0(flow_section, "│ │ ", sprintf("%-36s", "↓"), "│\n")
          flow_section <- paste0(flow_section, "│ ├─", sprintf("%-36s", first_line), "│\n")
        }
      }
      flow_section <- paste0(flow_section, "│ └", sprintf("%-37s", ""), "│\n")
    } else {
      flow_section <- "│ No code blocks detected               │\n"
    }
    
    # Footer
    footer <- paste0(
      "╰────────────────────────────────────╯\n\n",
      "Note: This is a text-based visualization.\n",
      "For graphical visualizations, install DiagrammeR: install.packages(\"DiagrammeR\")"
    )
    
    # Combine all parts
    viz <- paste0(
      header,
      flow_section,
      footer
    )
    
    return(viz)
  },
  
  #' @description Generate an ASCII script package visualization
  #' @param files Character vector of file paths to R scripts
  #' @return A character string with ASCII visualization
  generate_ascii_script_package = function(files) {
    # Extract script details
    script_data <- self$extract_script_metadata(files)
    
    # Collect all packages from all scripts
    all_packages <- character(0)
    for (file in files) {
      packages <- script_data[[file]]$packages
      all_packages <- union(all_packages, packages)
    }
    
    # Create header
    header <- "╭────── Script Package Usage ──────╮\n"
    
    # Create packages section
    packages_section <- ""
    
    if (length(all_packages) > 0) {
      # Group packages by category
      pkg_categories <- list(
        "Data Manipulation" = c("dplyr", "tidyr", "purrr", "plyr", "data.table"),
        "Visualization" = c("ggplot2", "plotly", "lattice", "ggvis", "highcharter"),
        "Machine Learning" = c("caret", "randomForest", "xgboost", "e1071", "nnet", "rpart"),
        "Statistics" = c("stats", "lme4", "nlme", "survival", "MASS", "car"),
        "I/O" = c("readr", "readxl", "haven", "jsonlite", "xml2", "httr")
      )
      
      # Track categorized packages
      categorized <- character(0)
      
      # Display packages by category
      for (cat_name in names(pkg_categories)) {
        cat_pkgs <- intersect(all_packages, pkg_categories[[cat_name]])
        if (length(cat_pkgs) > 0) {
          packages_section <- paste0(packages_section, "│ ", sprintf("%-35s", cat_name), " │\n")
          for (pkg in cat_pkgs) {
            # Find which scripts use this package
            used_in <- character(0)
            for (file in files) {
              if (pkg %in% script_data[[file]]$packages) {
                used_in <- c(used_in, basename(file))
              }
            }
            
            # Format the "used in" information
            used_in_text <- ""
            if (length(used_in) == length(files)) {
              used_in_text <- "(all scripts)"
            } else {
              used_in_text <- paste0("(", length(used_in), " scripts)")
            }
            
            packages_section <- paste0(packages_section, "│   ● ", sprintf("%-20s", pkg), 
                                      sprintf("%-13s", used_in_text), " │\n")
            categorized <- c(categorized, pkg)
          }
        }
      }
      
      # Add uncategorized packages
      uncategorized <- setdiff(all_packages, categorized)
      if (length(uncategorized) > 0) {
        packages_section <- paste0(packages_section, "│ ", sprintf("%-35s", "Other"), " │\n")
        for (pkg in uncategorized) {
          # Find which scripts use this package
          used_in <- character(0)
          for (file in files) {
            if (pkg %in% script_data[[file]]$packages) {
              used_in <- c(used_in, basename(file))
            }
          }
          
          # Format the "used in" information
          used_in_text <- ""
          if (length(used_in) == length(files)) {
            used_in_text <- "(all scripts)"
          } else {
            used_in_text <- paste0("(", length(used_in), " scripts)")
          }
          
          packages_section <- paste0(packages_section, "│   ● ", sprintf("%-20s", pkg), 
                                    sprintf("%-13s", used_in_text), " │\n")
        }
      }
    } else {
      packages_section <- "│ No packages detected                │\n"
    }
    
    # Footer
    footer <- paste0(
      "╰─────────────────────────────────╯\n\n",
      "Note: This is a text-based visualization.\n",
      "For graphical visualizations, install visNetwork: install.packages(\"visNetwork\")"
    )
    
    # Combine all parts
    viz <- paste0(
      header,
      packages_section,
      footer
    )
    
    return(viz)
  },
  
  #' @description Generate an ASCII script structure visualization
  #' @param files Character vector of file paths to R scripts
  #' @return A character string with ASCII visualization
  generate_ascii_script_structure = function(files) {
    # For structure visualization, focus on the first file if multiple files are provided
    file <- files[1]
    
    # Extract script content
    script_content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    functions <- self$extract_functions_defined(script_content)
    variables <- self$extract_variables(script_content)
    
    # Create header
    header <- paste0(
      "╭─ Script Structure: ", basename(file), " ─╮\n"
    )
    
    # Create functions section
    funcs_section <- "│ Functions:                          │\n"
    if (length(functions) > 0) {
      for (func_name in functions) {
        if (nchar(func_name) > 35) {
          func_name <- paste0(substr(func_name, 1, 32), "...")
        }
        funcs_section <- paste0(funcs_section, "│   ● ", sprintf("%-33s", func_name), "│\n")
      }
    } else {
      funcs_section <- paste0(funcs_section, "│   No functions defined              │\n")
    }
    
    # Create variables section
    vars_section <- "│ Key Variables:                      │\n"
    if (length(variables) > 0) {
      for (var_name in variables) {
        # Try to determine variable type
        var_type <- "unknown"
        if (grepl(paste0(var_name, "\\s*<-\\s*data\\.frame"), script_content)) {
          var_type <- "data.frame"
        } else if (grepl(paste0(var_name, "\\s*<-\\s*list"), script_content)) {
          var_type <- "list"
        } else if (grepl(paste0(var_name, "\\s*<-\\s*c\\("), script_content)) {
          var_type <- "vector"
        } else if (grepl(paste0(var_name, "\\s*<-\\s*\""), script_content) || 
                  grepl(paste0(var_name, "\\s*<-\\s*'"), script_content)) {
          var_type <- "character"
        } else if (grepl(paste0(var_name, "\\s*<-\\s*\\d"), script_content)) {
          var_type <- "numeric"
        } else if (grepl(paste0(var_name, "\\s*<-\\s*TRUE|", var_name, "\\s*<-\\s*FALSE"), script_content)) {
          var_type <- "logical"
        }
        
        var_display <- paste0(var_name, " (", var_type, ")")
        if (nchar(var_display) > 35) {
          var_display <- paste0(substr(var_display, 1, 32), "...")
        }
        vars_section <- paste0(vars_section, "│   ● ", sprintf("%-33s", var_display), "│\n")
      }
    } else {
      vars_section <- paste0(vars_section, "│   No key variables identified       │\n")
    }
    
    # Footer
    footer <- paste0(
      "╰────────────────────────────────────╯\n\n",
      "Note: This is a text-based visualization.\n",
      "For graphical visualizations, install DiagrammeR: install.packages(\"DiagrammeR\")"
    )
    
    # Combine all parts
    viz <- paste0(
      header,
      funcs_section,
      vars_section,
      footer
    )
    
    return(viz)
  },
  
  #' @description Extract metadata from scripts
  #' @param files Character vector of file paths
  #' @return A list with script metadata
  extract_script_metadata = function(files) {
    result <- list()
    
    for (file in files) {
      script_content <- paste(readLines(file, warn = FALSE), collapse = "\n")
      
      # Extract packages
      packages <- self$extract_packages(script_content)
      
      # Extract defined functions
      functions_defined <- self$extract_functions_defined(script_content)
      
      # Extract function calls
      functions_called <- self$extract_functions_called(script_content)
      
      # Extract variables
      variables <- self$extract_variables(script_content)
      
      # Store results
      result[[file]] <- list(
        packages = packages,
        functions_defined = functions_defined,
        functions_called = functions_called,
        variables = variables
      )
    }
    
    return(result)
  },
  
  #' @description Extract package dependencies from script content
  #' @param script_content Character string containing script content
  #' @return Character vector of package names
  extract_packages = function(script_content) {
    # Match library and require calls
    library_matches <- gregexpr("library\\s*\\(\\s*['\"]?([[:alnum:]\\._]*)\\s*['\"]?\\s*\\)", 
                              script_content, perl = TRUE)
    require_matches <- gregexpr("require\\s*\\(\\s*['\"]?([[:alnum:]\\._]*)\\s*['\"]?\\s*\\)",
                              script_content, perl = TRUE)
    
    # Extract package names from library calls
    library_packages <- character(0)
    if (library_matches[[1]][1] != -1) {
      match_strings <- regmatches(script_content, library_matches)[[1]]
      for (match in match_strings) {
        pkg <- gsub("library\\s*\\(\\s*['\"]?([[:alnum:]\\._]*)\\s*['\"]?\\s*\\)", "\\1", match, perl = TRUE)
        if (nchar(pkg) > 0) {
          library_packages <- c(library_packages, pkg)
        }
      }
    }
    
    # Extract package names from require calls
    require_packages <- character(0)
    if (require_matches[[1]][1] != -1) {
      match_strings <- regmatches(script_content, require_matches)[[1]]
      for (match in match_strings) {
        pkg <- gsub("require\\s*\\(\\s*['\"]?([[:alnum:]\\._]*)\\s*['\"]?\\s*\\)", "\\1", match, perl = TRUE)
        if (nchar(pkg) > 0) {
          require_packages <- c(require_packages, pkg)
        }
      }
    }
    
    # Also try to identify package usage through :: notation
    ns_matches <- gregexpr("([[:alnum:]\\._]+)::", script_content, perl = TRUE)
    ns_packages <- character(0)
    if (ns_matches[[1]][1] != -1) {
      match_strings <- regmatches(script_content, ns_matches)[[1]]
      for (match in match_strings) {
        pkg <- gsub("([[:alnum:]\\._]+)::", "\\1", match, perl = TRUE)
        if (nchar(pkg) > 0 && !pkg %in% c("base", "utils", "stats", "methods", "graphics", "grDevices")) {
          ns_packages <- c(ns_packages, pkg)
        }
      }
    }
    
    # Combine all packages and remove duplicates
    packages <- unique(c(library_packages, require_packages, ns_packages))
    
    return(packages)
  },
  
  #' @description Extract defined functions from script content
  #' @param script_content Character string containing script content
  #' @return Character vector of function names
  extract_functions_defined = function(script_content) {
    # Match function definitions
    func_matches <- gregexpr("([[:alnum:]\\._]+)\\s*<-\\s*function\\s*\\(", script_content, perl = TRUE)
    
    # Extract function names
    functions <- character(0)
    if (func_matches[[1]][1] != -1) {
      match_strings <- regmatches(script_content, func_matches)[[1]]
      for (match in match_strings) {
        func_name <- gsub("([[:alnum:]\\._]+)\\s*<-\\s*function\\s*\\(", "\\1", match, perl = TRUE)
        if (nchar(func_name) > 0) {
          functions <- c(functions, func_name)
        }
      }
    }
    
    return(functions)
  },
  
  #' @description Extract function calls from script content
  #' @param script_content Character string containing script content
  #' @return Character vector of function names
  extract_functions_called = function(script_content) {
    # Match function calls
    func_matches <- gregexpr("([[:alnum:]\\._]+)\\s*\\(", script_content, perl = TRUE)
    
    # Extract function names
    functions <- character(0)
    if (func_matches[[1]][1] != -1) {
      match_strings <- regmatches(script_content, func_matches)[[1]]
      for (match in match_strings) {
        func_name <- gsub("([[:alnum:]\\._]+)\\s*\\(", "\\1", match, perl = TRUE)
        # Exclude control flow keywords
        if (nchar(func_name) > 0 && !func_name %in% c("if", "for", "while", "function")) {
          functions <- c(functions, func_name)
        }
      }
    }
    
    return(unique(functions))
  },
  
  #' @description Extract variables from script content
  #' @param script_content Character string containing script content
  #' @return Character vector of variable names
  extract_variables = function(script_content) {
    # Match variable assignments
    var_matches <- gregexpr("([[:alnum:]\\._]+)\\s*<-\\s*(?!function)", script_content, perl = TRUE)
    
    # Extract variable names
    variables <- character(0)
    if (var_matches[[1]][1] != -1) {
      match_strings <- regmatches(script_content, var_matches)[[1]]
      for (match in match_strings) {
        var_name <- gsub("([[:alnum:]\\._]+)\\s*<-\\s*", "\\1", match, perl = TRUE)
        if (nchar(var_name) > 0) {
          variables <- c(variables, var_name)
        }
      }
    }
    
    return(unique(variables))
  },
  
  #' @description Extract code blocks from script content
  #' @param script_content Character string containing script content
  #' @return Character vector of code blocks
  extract_code_blocks = function(script_content) {
    # Split script into logical blocks
    # Simple approach: split by groups of newlines or comments
    lines <- strsplit(script_content, "\n")[[1]]
    
    # Remove empty lines at the beginning and end
    while (length(lines) > 0 && nchar(trimws(lines[1])) == 0) {
      lines <- lines[-1]
    }
    while (length(lines) > 0 && nchar(trimws(lines[length(lines)])) == 0) {
      lines <- lines[-length(lines)]
    }
    
    # Initialize blocks
    blocks <- character(0)
    current_block <- character(0)
    
    # Process lines
    for (i in seq_along(lines)) {
      line <- lines[i]
      trimmed <- trimws(line)
      
      # Check if this is a separator (empty line or comment)
      is_separator <- nchar(trimmed) == 0 || grepl("^#", trimmed)
      
      # If we have content in the current block and hit a separator
      if (length(current_block) > 0 && is_separator) {
        # If this is one of multiple consecutive separators, add to the current block
        if (length(blocks) == 0 || length(current_block) >= 3) {
          # Finish the current block
          blocks <- c(blocks, paste(current_block, collapse = "\n"))
          current_block <- character(0)
        }
      }
      
      # Add the current line to the accumulating block
      current_block <- c(current_block, line)
      
      # If this is the last line, add the final block
      if (i == length(lines) && length(current_block) > 0) {
        blocks <- c(blocks, paste(current_block, collapse = "\n"))
      }
    }
    
    # Merge very small blocks (1-2 lines) with the previous block
    if (length(blocks) > 1) {
      merged_blocks <- character(0)
      current_merged <- blocks[1]
      
      for (i in 2:length(blocks)) {
        block_lines <- length(strsplit(blocks[i], "\n")[[1]])
        
        if (block_lines <= 2) {
          # Merge with previous block
          current_merged <- paste(current_merged, blocks[i], sep = "\n")
        } else {
          # Save the current merged block and start a new one
          merged_blocks <- c(merged_blocks, current_merged)
          current_merged <- blocks[i]
        }
      }
      
      # Add the last merged block
      merged_blocks <- c(merged_blocks, current_merged)
      blocks <- merged_blocks
    }
    
    return(blocks)
  }
)

# Add script visualization methods to VisualizationHandler
inject_script_visualization_methods <- function() {
  # Get the global VisualizationHandler class object
  if (!exists("VisualizationHandler")) {
    warning("VisualizationHandler class not found. Script visualization methods not added.")
    return(FALSE)
  }
  
  # Add script visualization support to get_required_packages method
  old_required_pkgs_fn <- VisualizationHandler$public_methods$get_required_packages
  VisualizationHandler$public_methods$get_required_packages <- function(type = "diagram") {
    if (type %in% c("script_dependency", "script_package")) {
      return(c("visNetwork", "igraph"))
    } else if (type %in% c("script_flow", "script_structure")) {
      return(c("DiagrammeR"))
    } else {
      return(old_required_pkgs_fn(type))
    }
  }
  
  # Loop through the extension methods and add them to the class
  for (method_name in names(VisualizationHandlerScriptExtension)) {
    VisualizationHandler$public_methods[[method_name]] <- VisualizationHandlerScriptExtension[[method_name]]
  }
  
  return(TRUE)
}

# Run the method injection after the package loads
.onLoad <- function(libname, pkgname) {
  # This will be called when the package is loaded
  inject_script_visualization_methods()
}