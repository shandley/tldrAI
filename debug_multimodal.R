# Debugging script for multimodal visualizations

# Load required packages
library(htmlwidgets)
library(htmltools)
library(DiagrammeR)
if (requireNamespace("visNetwork", quietly = TRUE)) {
  library(visNetwork)
}
if (requireNamespace("highlight", quietly = TRUE)) {
  library(highlight)
}

# Source the multimodal visualization code
source("R/multimodal_visualization.R")

# Mock metadata for testing
metadata <- list(
  package = "dplyr",
  args = c("data", "...", ".by", ".preserve"),
  returns = "A filtered data frame"
)

# Test function to call each visualization function directly
test_visualizations <- function() {
  # Test function diagram
  cat("\n\n==== Testing function diagram ====\n")
  diagram <- tryCatch({
    viz <- generate_function_diagram_panel("filter", metadata, "light")
    cat("Diagram class:", class(viz), "\n")
    if (inherits(viz, "htmlwidget")) {
      temp_file <- tempfile(fileext = ".html")
      saveWidget(viz, temp_file, selfcontained = TRUE)
      cat("Saved diagram to:", temp_file, "\n")
      cat("File size:", file.info(temp_file)$size, "bytes\n")
      
      # View in browser
      browseURL(temp_file)
    }
    "Diagram test completed"
  }, error = function(e) {
    cat("ERROR in diagram:", e$message, "\n")
    return(NULL)
  })
  
  # Test function network
  cat("\n\n==== Testing function network ====\n")
  network <- tryCatch({
    viz <- generate_function_network_panel("filter", metadata, "light")
    cat("Network class:", class(viz), "\n")
    if (inherits(viz, "htmlwidget")) {
      temp_file <- tempfile(fileext = ".html")
      saveWidget(viz, temp_file, selfcontained = TRUE)
      cat("Saved network to:", temp_file, "\n")
      cat("File size:", file.info(temp_file)$size, "bytes\n")
      
      # View in browser
      browseURL(temp_file)
    }
    "Network test completed"
  }, error = function(e) {
    cat("ERROR in network:", e$message, "\n")
    return(NULL)
  })
  
  # Test code highlight
  cat("\n\n==== Testing code highlight ====\n")
  highlight <- tryCatch({
    viz <- generate_code_highlight_panel("filter", metadata, "light")
    cat("Highlight class:", class(viz), "\n")
    if (inherits(viz, "shiny.tag")) {
      temp_file <- tempfile(fileext = ".html")
      html_content <- as.character(doRenderTags(viz))
      writeLines(html_content, temp_file)
      cat("Saved highlight to:", temp_file, "\n")
      cat("File size:", file.info(temp_file)$size, "bytes\n")
      
      # View in browser
      browseURL(temp_file)
    }
    "Highlight test completed"
  }, error = function(e) {
    cat("ERROR in highlight:", e$message, "\n")
    return(NULL)
  })
  
  # Test data flow
  cat("\n\n==== Testing data flow ====\n")
  dataflow <- tryCatch({
    viz <- generate_data_transformation_panel("filter", metadata, theme = "light")
    cat("Data flow class:", class(viz), "\n")
    if (inherits(viz, "shiny.tag")) {
      temp_file <- tempfile(fileext = ".html")
      html_content <- as.character(doRenderTags(viz))
      writeLines(html_content, temp_file)
      cat("Saved data flow to:", temp_file, "\n")
      cat("File size:", file.info(temp_file)$size, "bytes\n")
      
      # View in browser
      browseURL(temp_file)
    }
    "Data flow test completed"
  }, error = function(e) {
    cat("ERROR in data flow:", e$message, "\n")
    return(NULL)
  })
  
  # Test complete widget with all visualizations
  cat("\n\n==== Testing full multimodal widget ====\n")
  widget <- tryCatch({
    response_text <- paste0(
      "# filter\n\n",
      "## Purpose\n",
      "Filters rows from a data frame based on specified conditions.\n\n",
      "## Usage\n",
      "```r\n",
      "filter(data, ..., .by = NULL, .preserve = FALSE)\n",
      "```\n\n",
      "## Arguments\n",
      "- `data`: A data frame, data frame extension (e.g. a tibble), or a lazy data frame\n",
      "- `...`: Logical expressions that filter the data based on the variables\n",
      "- `.by`: Variables to group by for this operation\n",
      "- `.preserve`: If TRUE, preserves grouping structure\n\n",
      "## Examples\n",
      "```r\n",
      "library(dplyr)\n",
      "starwars %>% filter(species == \"Human\")\n",
      "starwars %>% filter(species == \"Human\", mass > 50)\n",
      "```\n"
    )

    widget <- tldr_multimodal(
      func_name = "dplyr::filter",
      metadata = metadata,
      response_text = response_text,
      theme = "light",
      visualization_types = c("diagram", "data_flow", "function_network", "code_highlight")
    )
    
    cat("Widget class:", class(widget), "\n")
    
    # Save and view the complete widget
    temp_file <- tempfile(fileext = ".html")
    saveWidget(widget, temp_file, selfcontained = TRUE)
    cat("Saved full widget to:", temp_file, "\n")
    cat("File size:", file.info(temp_file)$size, "bytes\n")
    
    # View in browser
    browseURL(temp_file)
    
    "Full widget test completed"
  }, error = function(e) {
    cat("ERROR in full widget:", e$message, "\n")
    return(NULL)
  })
  
  # Return results
  list(
    diagram = diagram,
    network = network,
    highlight = highlight,
    dataflow = dataflow,
    widget = widget
  )
}

# Run the tests
results <- test_visualizations()
print(results)