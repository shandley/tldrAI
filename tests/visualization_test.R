# Direct testing of visualization functionality
# This file tests all visualization functions in the tldrAI package
cat("Loading required packages...\n")
library(DiagrammeR)
library(visNetwork)
library(highlight)
library(htmltools)

# Add get_config placeholder
get_config <- function(key, default = NULL) {
  return(default)
}

# Add null coalescing operator if not available
if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

# Source implementation files directly
# (Comment these out if testing within the installed package)
cat("Sourcing visualization implementation files...\n")
source("/Users/scott/gdrive/code/R/tldrAI/R/visualization_impl.R", local = TRUE)
source("/Users/scott/gdrive/code/R/tldrAI/R/visualization_core.R", local = TRUE)
source("/Users/scott/gdrive/code/R/tldrAI/R/simple_visualizations.R", local = TRUE)

# Test data
cat("Creating test metadata...\n")
test_metadata_list <- list(
  filter = list(
    name = "filter",
    package = "dplyr",
    args = c(".data", "...", ".by", ".preserve"),
    signature = "filter(.data, ..., .by = NULL, .preserve = FALSE)",
    description = "Subset rows of a data frame based on specified conditions."
  ),
  ggplot = list(
    name = "ggplot",
    package = "ggplot2",
    args = c("data", "mapping", "...", "environment"),
    signature = "ggplot(data = NULL, mapping = aes(), ..., environment = parent.frame())",
    description = "Create a new ggplot object."
  ),
  mean = list(
    name = "mean",
    package = "base",
    args = c("x", "trim", "na.rm", "..."),
    signature = "mean(x, trim = 0, na.rm = FALSE, ...)",
    description = "Calculate the arithmetic mean."
  ),
  lm = list(
    name = "lm",
    package = "stats",
    args = c("formula", "data", "subset", "weights", "na.action", "method", "model", "x", "y", "qr", "singular.ok", "contrasts", "offset", "..."),
    signature = "lm(formula, data, subset, weights, na.action, method = \"qr\", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, ...)",
    description = "Fit linear models."
  )
)

# Test each visualization type
cat("\n=== Testing visualizations ===\n\n")

# Test for all examples
for (func_name in names(test_metadata_list)) {
  metadata <- test_metadata_list[[func_name]]
  
  cat(paste0("\n=== Testing visualizations for ", func_name, " (", metadata$package, ") ===\n"))
  
  cat("\nBasic diagram...\n")
  tryCatch({
    diagram <- generate_diagram(func_name, metadata)
    print(diagram)
  }, error = function(e) {
    cat("Error generating diagram:", e$message, "\n")
  })
  
  cat("\nFlowchart diagram...\n")
  tryCatch({
    flowchart <- generate_flowchart(func_name, metadata)
    print(flowchart)
  }, error = function(e) {
    cat("Error generating flowchart:", e$message, "\n")
  })
  
  cat("\nData flow diagram...\n")
  tryCatch({
    data_flow <- generate_data_flow_diagram(func_name, metadata)
    print(data_flow)
  }, error = function(e) {
    cat("Error generating data flow:", e$message, "\n")
  })
  
  cat("\nFunction network...\n")
  tryCatch({
    network <- generate_function_network(func_name, metadata)
    print(network)
  }, error = function(e) {
    cat("Error generating function network:", e$message, "\n")
  })
  
  cat("\nCode highlight...\n")
  tryCatch({
    highlight <- generate_code_highlight(func_name, metadata)
    print(highlight)
  }, error = function(e) {
    cat("Error generating code highlight:", e$message, "\n")
  })
  
  # Test export
  cat("\nTesting SVG export...\n")
  if (require(DiagrammeRsvg, quietly = TRUE)) {
    export_file <- paste0(func_name, "_diagram.svg")
    tryCatch({
      export_visualization(diagram, export_file, format = "svg")
      cat("Exported to", export_file, "\n")
    }, error = function(e) {
      cat("Error exporting diagram:", e$message, "\n")
    })
  } else {
    cat("DiagrammeRsvg package not available, skipping export test\n")
  }
  
  # Test HTML export for function network (visNetwork)
  cat("\nTesting HTML export for function network...\n")
  export_file <- paste0(func_name, "_network.html")
  tryCatch({
    export_visualization(network, export_file, format = "html")
    cat("Exported to", export_file, "\n")
  }, error = function(e) {
    cat("Error exporting network:", e$message, "\n")
  })
}

# Test creating visualizations through the main interface
cat("\n=== Testing visualization creation interface ===\n\n")

for (vis_type in c("diagram", "flowchart", "data_flow", "function_network", "code_highlight")) {
  func_name <- "mean"
  metadata <- test_metadata_list[["mean"]]
  cat("\nTesting creation of", vis_type, "visualization...\n")
  
  tryCatch({
    viz <- create_visualization(func_name, metadata, vis_type = vis_type, prompt_install = FALSE)
    print(viz)
    cat("Successfully created", vis_type, "visualization\n")
  }, error = function(e) {
    cat("Error creating", vis_type, "visualization:", e$message, "\n")
  })
}

cat("\n=== Testing ASCII fallbacks ===\n\n")

# Test all ASCII fallbacks
func_name <- "mean"
metadata <- test_metadata_list[["mean"]]

cat("ASCII diagram visualization:\n")
ascii_diagram <- generate_ascii_diagram(func_name, metadata)
cat(ascii_diagram, "\n\n")

cat("ASCII flowchart visualization:\n")
ascii_flowchart <- generate_ascii_flowchart(func_name, metadata)
cat(ascii_flowchart, "\n\n")

cat("ASCII data flow visualization:\n")
ascii_data_flow <- generate_ascii_data_flow(func_name, metadata)
cat(ascii_data_flow, "\n\n")

cat("ASCII function network visualization:\n")
ascii_network <- generate_ascii_function_network(func_name, metadata)
cat(ascii_network, "\n\n")

cat("ASCII code highlight visualization:\n")
ascii_highlight <- generate_ascii_code_highlight(func_name, metadata)
cat(ascii_highlight, "\n\n")

cat("\nAll tests complete.\n")