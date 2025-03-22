# Core visualization functions

# Main function to create a visualization
create_visualization <- function(func_name, metadata, vis_type = "diagram", prompt_install = TRUE) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Creating visualization with type: ", vis_type)
  }
  
  # Check for required visualization packages
  has_diagrammer <- requireNamespace("DiagrammeR", quietly = TRUE)
  has_visnetwork <- requireNamespace("visNetwork", quietly = TRUE)
  has_highlight <- requireNamespace("highlight", quietly = TRUE)
  
  # If packages not available, prompt for installation if allowed
  if (prompt_install && (!has_diagrammer || !has_visnetwork || !has_highlight)) {
    missing_pkgs <- c()
    if (!has_diagrammer) missing_pkgs <- c(missing_pkgs, "DiagrammeR", "DiagrammeRsvg")
    if (!has_visnetwork) missing_pkgs <- c(missing_pkgs, "visNetwork")
    if (!has_highlight) missing_pkgs <- c(missing_pkgs, "highlight")
    
    if (length(missing_pkgs) > 0) {
      install_cmd <- paste0('install.packages(c("', paste(missing_pkgs, collapse = '", "'), '"))')
      
      # Check if in interactive mode
      if (interactive()) {
        message("To enable advanced visualizations, please install the following packages:")
        message(install_cmd)
        
        # Ask user if they want to install
        response <- readline("Would you like to install these packages now? (y/n): ")
        if (tolower(response) == "y" || tolower(response) == "yes") {
          tryCatch({
            eval(parse(text = install_cmd))
            message("Packages installed successfully!")
            
            # Recheck packages
            has_diagrammer <- requireNamespace("DiagrammeR", quietly = TRUE)
            has_visnetwork <- requireNamespace("visNetwork", quietly = TRUE)
            has_highlight <- requireNamespace("highlight", quietly = TRUE)
          }, error = function(e) {
            message("Failed to install packages: ", e$message)
            message("Falling back to ASCII visualizations")
            return(generate_ascii_visualization(func_name, metadata, vis_type))
          })
        } else {
          message("Using ASCII fallback visualizations")
          return(generate_ascii_visualization(func_name, metadata, vis_type))
        }
      } else {
        message("Non-interactive session. Using ASCII fallback visualizations")
        message("To enable advanced visualizations, run: ", install_cmd)
        return(generate_ascii_visualization(func_name, metadata, vis_type))
      }
    }
  }
  
  # Dispatch to appropriate visualization function based on type
  if (vis_type == "diagram") {
    # Basic function diagram
    if (has_diagrammer) {
      return(generate_diagram(func_name, metadata))
    } else {
      message("DiagrammeR package not available, using ASCII fallback for diagram")
      return(generate_ascii_diagram(func_name, metadata))
    }
  } else if (vis_type == "flowchart") {
    # Flowchart visualization
    if (has_diagrammer) {
      return(generate_flowchart(func_name, metadata))
    } else {
      message("DiagrammeR package not available, using ASCII fallback for flowchart")
      return(generate_ascii_flowchart(func_name, metadata))
    }
  } else if (vis_type == "data_flow") {
    # Data flow visualization
    if (has_diagrammer) {
      return(generate_data_flow_diagram(func_name, metadata))
    } else {
      message("DiagrammeR package not available, using ASCII fallback for data flow")
      return(generate_ascii_data_flow(func_name, metadata))
    }
  } else if (vis_type == "function_network") {
    # Function network visualization
    if (has_visnetwork) {
      return(generate_function_network(func_name, metadata))
    } else {
      message("visNetwork package not available, using ASCII fallback for function network")
      return(generate_ascii_function_network(func_name, metadata))
    }
  } else if (vis_type == "code_highlight") {
    # Code highlight visualization
    if (has_highlight) {
      return(generate_code_highlight(func_name, metadata))
    } else {
      message("highlight package not available, using ASCII fallback for code highlight")
      return(generate_ascii_code_highlight(func_name, metadata))
    }
  } else {
    # Unknown visualization type, fallback to ASCII
    message("Unknown visualization type: ", vis_type, ", using diagram visualization")
    if (has_diagrammer) {
      return(generate_diagram(func_name, metadata))
    } else {
      return(generate_ascii_diagram(func_name, metadata))
    }
  }
}

#' Export a visualization to a file
#'
#' @param visualization The visualization object to export
#' @param file_path The path where the file should be saved
#' @param format The format to save in ("svg", "png", "html", "txt")
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise
#' @keywords internal
export_visualization <- function(visualization, file_path, format = NULL) {
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Exporting visualization to: ", file_path)
  }
  
  # Determine format if not provided
  if (is.null(format)) {
    # Extract extension from file_path
    ext <- tolower(tools::file_ext(file_path))
    
    if (ext %in% c("svg", "png", "html", "txt")) {
      format <- ext
    } else {
      # Default to SVG
      format <- "svg"
      file_path <- paste0(tools::file_path_sans_ext(file_path), ".", format)
      message("No format specified, defaulting to svg: ", file_path)
    }
  } else {
    # Ensure file has the correct extension
    ext <- tolower(tools::file_ext(file_path))
    if (ext != format) {
      file_path <- paste0(tools::file_path_sans_ext(file_path), ".", format)
      message("Updating file extension to match format: ", file_path)
    }
  }
  
  # Create parent directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Export based on the visualization type and requested format
  if (is.character(visualization)) {
    # ASCII visualization - save as text file
    tryCatch({
      writeLines(visualization, file_path)
      message("Visualization exported to: ", file_path)
      return(invisible(TRUE))
    }, error = function(e) {
      message("Error exporting visualization: ", e$message)
      return(invisible(FALSE))
    })
  } else if (inherits(visualization, "htmlwidget")) {
    # Interactive visualizations (visNetwork, htmlwidgets)
    if (format == "html") {
      # Save directly as HTML
      tryCatch({
        htmlwidgets::saveWidget(visualization, file_path, selfcontained = TRUE)
        message("Visualization exported to: ", file_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    } else if (format == "png") {
      # Save as PNG using webshot
      if (!requireNamespace("webshot", quietly = TRUE)) {
        message("The 'webshot' package is required to export as PNG. Please install it with: install.packages('webshot')")
        return(invisible(FALSE))
      }
      
      tryCatch({
        temp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(visualization, temp_html, selfcontained = TRUE)
        webshot::webshot(temp_html, file_path, delay = 0.5)
        unlink(temp_html)
        message("Visualization exported to: ", file_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    } else {
      # For other formats, save as HTML and notify user
      tryCatch({
        html_path <- paste0(tools::file_path_sans_ext(file_path), ".html")
        htmlwidgets::saveWidget(visualization, html_path, selfcontained = TRUE)
        message("Interactive visualization cannot be directly exported to ", format, ". Saved as HTML instead: ", html_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    }
  } else if (inherits(visualization, "grViz") || inherits(visualization, "DiagrammeR")) {
    # DiagrammeR visualizations
    if (format == "svg") {
      # Export as SVG
      if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
        message("The 'DiagrammeRsvg' package is required to export as SVG. Please install it with: install.packages('DiagrammeRsvg')")
        return(invisible(FALSE))
      }
      
      tryCatch({
        svg_code <- DiagrammeRsvg::export_svg(visualization)
        writeLines(svg_code, file_path)
        message("Visualization exported to: ", file_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    } else if (format == "png") {
      # Export as PNG
      if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) || !requireNamespace("rsvg", quietly = TRUE)) {
        message("The 'DiagrammeRsvg' and 'rsvg' packages are required to export as PNG. Please install them with: install.packages(c('DiagrammeRsvg', 'rsvg'))")
        return(invisible(FALSE))
      }
      
      tryCatch({
        svg_code <- DiagrammeRsvg::export_svg(visualization)
        rsvg::rsvg_png(charToRaw(svg_code), file_path)
        message("Visualization exported to: ", file_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    } else if (format == "html") {
      # Save as HTML
      tryCatch({
        # Create a basic HTML file with the widget
        html_content <- paste0(
          "<!DOCTYPE html>\n<html>\n<head>\n",
          "<title>tldrAI Visualization</title>\n",
          "</head>\n<body>\n",
          "<div id='visualization'>\n",
          DiagrammeRsvg::export_svg(visualization),
          "\n</div>\n</body>\n</html>"
        )
        writeLines(html_content, file_path)
        message("Visualization exported to: ", file_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    } else {
      message("Format '", format, "' not supported for DiagrammeR visualizations. Try 'svg', 'png', or 'html'.")
      return(invisible(FALSE))
    }
  } else if (inherits(visualization, "shiny.tag") || inherits(visualization, "html")) {
    # HTML content (like highlighted code)
    if (format == "html") {
      tryCatch({
        html_content <- as.character(visualization)
        writeLines(html_content, file_path)
        message("Visualization exported to: ", file_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    } else {
      message("HTML content can only be exported to HTML format. Saving as HTML instead.")
      html_path <- paste0(tools::file_path_sans_ext(file_path), ".html")
      tryCatch({
        html_content <- as.character(visualization)
        writeLines(html_content, html_path)
        message("Visualization exported to: ", html_path)
        return(invisible(TRUE))
      }, error = function(e) {
        message("Error exporting visualization: ", e$message)
        return(invisible(FALSE))
      })
    }
  } else {
    # Unknown object type
    message("Unable to export visualization of type: ", class(visualization)[1])
    return(invisible(FALSE))
  }
}