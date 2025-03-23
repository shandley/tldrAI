# Example script showing the usage of the multimodal visualization interface

# Load required packages
if (!requireNamespace("markdown", quietly = TRUE)) {
  install.packages("markdown", repos = "https://cloud.r-project.org/")
}
if (!requireNamespace("htmltools", quietly = TRUE)) {
  install.packages("htmltools", repos = "https://cloud.r-project.org/")
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  install.packages("htmlwidgets", repos = "https://cloud.r-project.org/")
}
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR", repos = "https://cloud.r-project.org/")
}

# Load the library
library(tldrAI)

# Configure API key (replace with your own)
tldr_config(api_key = "YOUR_CLAUDE_API_KEY")

# Basic usage of multimodal visualization with dplyr's filter function
tldr("dplyr::filter", multimodal = TRUE)

# Using a dark theme
tldr("ggplot2::ggplot", multimodal = TRUE, theme = "dark")

# Configure default multimodal settings
tldr_multimodal_config(
  enable_multimodal = TRUE,
  theme = "light",
  default_height = 700,
  # Include all visualization types to test the interface
  default_visualizations = c("diagram", "data_flow", "function_network", "code_highlight")
)

# Now multimodal is enabled by default
tldr("mean")  # Will use multimodal interface with above settings

# Test standalone multimodal visualization
#--------------------------------------------------
# Direct usage of the multimodal function
# (useful if you already have function metadata and documentation)
metadata <- list(
  name = "filter",
  package = "dplyr",
  args = c("data", "...", ".by", ".preserve"),
  description = "Filter rows that match given conditions",
  returns = "A data frame with matching rows"
)

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

custom_multimodal <- tldr_multimodal(
  func_name = "dplyr::filter",
  metadata = metadata,
  response_text = response_text,
  theme = "light",
  visualization_types = c("diagram", "data_flow")
)

print(custom_multimodal)

#--------------------------------------------------
# Test with a simple HTML rendering approach
manual_html <- '<div style="padding: 20px; font-family: Arial, sans-serif;">
  <h1 style="color: #4285F4;">filter</h1>
  <h2 style="color: #34A853;">Purpose</h2>
  <p>Filters rows from a data frame based on specified conditions.</p>
  <h2 style="color: #34A853;">Usage</h2>
  <pre style="background-color: #F5F7F9; padding: 10px; border-radius: 4px;"><code>filter(data, condition1, condition2, ...)</code></pre>
  <h2 style="color: #34A853;">Key Arguments</h2>
  <p><strong style="color: #4285F4;">• data:</strong> The data frame to filter</p>
  <p><strong style="color: #4285F4;">• condition1, condition2, ...:</strong> Logical expressions that determine which rows to keep</p>
  <h2 style="color: #34A853;">Examples</h2>
  <pre style="background-color: #F5F7F9; padding: 10px; border-radius: 4px;"><code># Filter mtcars for cars with mpg > 20
filter(mtcars, mpg > 20)

# Filter iris for setosa species with long sepals
filter(iris, Species == "setosa", Sepal.Length > 5)</code></pre>
</div>'

html_multimodal <- tldr_multimodal(
  func_name = "dplyr::filter",
  metadata = metadata,
  response_text = htmltools::HTML(manual_html),
  theme = "light",
  visualization_types = c("diagram", "data_flow")
)

print(html_multimodal)

#--------------------------------------------------
# Test with DiagrammeR visualizations
#--------------------------------------------------
# Install DiagrammeR if not already installed
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR", repos = "https://cloud.r-project.org/")
}

# Test with dplyr function and diagram visualization
dplyr_viz <- tldr(
  "dplyr::filter", 
  multimodal = TRUE,
  visualization_types = c("diagram", "function_network", "code_highlight")
)

# Test with ggplot2 function with all visualization types
ggplot_viz <- tldr(
  "ggplot2::ggplot", 
  multimodal = TRUE,
  visualization_types = c("diagram", "data_flow", "function_network", "code_highlight"),
  theme = "dark"  # Test dark theme
)

# Test with base R function
base_viz <- tldr(
  "mean", 
  multimodal = TRUE,
  visualization_types = c("diagram", "data_flow", "function_network", "code_highlight")
)

# Display the visualizations
print(dplyr_viz)
print(ggplot_viz)
print(base_viz)