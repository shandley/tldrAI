# Examples for testing tldrAI visualizations

# Load required libraries
library(tldrAI)

# Basic usage with visualization
tldr("mean", visualize = TRUE)

# Try different visualization types
tldr("filter", package = "dplyr", vis_type = "diagram")
tldr("filter", package = "dplyr", vis_type = "flowchart")
tldr("filter", package = "dplyr", vis_type = "data_flow")
tldr("filter", package = "dplyr", vis_type = "function_network")
tldr("filter", package = "dplyr", vis_type = "code_highlight")

# Test different functions from various packages
tldr("ggplot", package = "ggplot2", vis_type = "data_flow")
tldr("lm", package = "stats", vis_type = "function_network")
tldr("read.csv", package = "utils", vis_type = "flowchart")

# Export visualizations to files
tldr("mean", visualize = TRUE, export_visualization = TRUE, 
     export_path = "mean_diagram.svg")
tldr("filter", package = "dplyr", vis_type = "function_network", 
     export_visualization = TRUE, export_path = "filter_network.html")
tldr("ggplot", package = "ggplot2", vis_type = "data_flow", 
     export_visualization = TRUE, export_path = "ggplot_flow.png", 
     export_format = "png")

# Run individual visualization functions directly
# (These require the metadata to be properly structured)
library(DiagrammeR)
library(visNetwork)
library(highlight)
library(htmltools)

# Create test metadata
test_metadata <- list(
  name = "mean",
  package = "base",
  args = c("x", "trim", "na.rm", "..."),
  signature = "mean(x, trim = 0, na.rm = FALSE, ...)",
  description = "Calculate the arithmetic mean."
)

# Test individual visualizations
diagram <- generate_diagram("mean", test_metadata)
print(diagram)

flowchart <- generate_flowchart("mean", test_metadata)
print(flowchart)

data_flow <- generate_data_flow_diagram("mean", test_metadata)
print(data_flow)

network <- generate_function_network("mean", test_metadata)
print(network)

code_highlight <- generate_code_highlight("mean", test_metadata)
print(code_highlight)

# Test ASCII fallbacks
ascii_diagram <- generate_ascii_diagram("mean", test_metadata)
cat(ascii_diagram)

# Export a visualization
export_visualization(diagram, "mean_diagram_export.svg")