# tldrAI

> AI-Powered Quick Reference for R Functions

## Overview

`tldrAI` modernizes the concept of the tldr package with AI capabilities. It provides concise, practical help for R functions using LLMs while maintaining the brevity and example-focused philosophy of tldr.

## Installation

```r
# Install from GitHub
# devtools::install_github("username/tldrAI")
```

## Usage

```r
library(tldrAI)

# Configure your API key (do this once)
tldr_config(api_key = "your_claude_api_key")

# Get concise help for a function
tldr("mean")

# Get more detailed help with more examples
tldr("ggplot", verbose = TRUE, examples = 3)

# Clear the cache
tldr_cache_clear()
```

## Features

- **Concise Help**: Get straight-to-the-point explanations focused on practical usage
- **Examples-First**: Prioritizes realistic code examples over verbose documentation
- **AI-Powered**: Uses Claude's API to generate context-aware, helpful explanations
- **Customizable**: Configure verbosity, number of examples, and other preferences
- **Response Caching**: Cache responses to reduce API calls and improve speed

## Configuration

You can configure `tldrAI` using the `tldr_config()` function:

```r
tldr_config(
  api_key = "your_claude_api_key",
  model = "claude-3-opus-20240229",  # Set a different Claude model
  cache_enabled = TRUE,              # Enable/disable response caching
  verbose_default = FALSE,           # Default verbosity setting
  examples_default = 2               # Default number of examples
)
```

## License

MIT
