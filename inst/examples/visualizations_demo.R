# Demo script for tldrAI visualizations
library(tldrAI)

# Turn on debug mode
tldr_debug(TRUE)

# Create a basic diagram
cat("\n\nTesting diagram visualization for 'mean'...\n")
tldr("base::mean", visualize = TRUE)

# Create a data flow diagram
cat("\n\nTesting data flow visualization for 'dplyr::filter'...\n")
tldr("dplyr::filter", vis_type = "data_flow", visualize = TRUE)

# Create a function network
cat("\n\nTesting function network visualization for 'stats::lm'...\n")
tldr("stats::lm", vis_type = "function_network", visualize = TRUE)

# Create a code highlight visualization
cat("\n\nTesting code highlight visualization for 'base::lapply'...\n")
tldr("base::lapply", vis_type = "code_highlight", visualize = TRUE)

# Export a visualization
cat("\n\nExporting visualization to file...\n")
tldr("mean", visualize = TRUE, 
     export_visualization = TRUE, 
     export_path = "mean_diagram.svg")

# Turn off debug mode
tldr_debug(FALSE)