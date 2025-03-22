# Example script showing the usage of the multimodal visualization interface

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
  default_visualizations = c("diagram", "data_flow", "code_highlight")
)

# Now multimodal is enabled by default
tldr("mean")  # Will use multimodal interface with above settings

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
  visualization_types = c("diagram", "data_flow", "function_network")
)

print(custom_multimodal)