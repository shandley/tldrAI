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
# Or use OpenAI
tldr_config(openai_api_key = "your_openai_api_key", provider = "openai")

# Get concise help for a function
tldr("mean")

# Get more detailed help with more examples
tldr("ggplot", verbose = TRUE, examples = 3)

# Switch providers for a specific query
tldr("dplyr::filter", provider = "openai") 

# Enable offline mode
tldr_offline(TRUE)

# Configure fallback provider
tldr_config(enable_fallback = TRUE, fallback_provider = "openai")

# Clear the cache
tldr_cache_clear()

# Clear only expired cache entries
tldr_cache_clear(expired_only = TRUE)

# Test connection to a provider
tldr_test_connection("claude")
```

## Features

- **Concise Help**: Get straight-to-the-point explanations focused on practical usage
- **Examples-First**: Prioritizes realistic code examples over verbose documentation
- **Multiple LLM Providers**: Support for both Claude and OpenAI APIs
- **Provider Fallback**: Automatically switch to a fallback provider if the primary one fails
- **Offline Mode**: Use cached responses when you're offline
- **Cache Management**: Intelligent caching with TTL and selective clearing
- **Customizable**: Configure verbosity, number of examples, and other preferences
- **Package Functions**: Support for functions from any package with `package::function` syntax

## Configuration

You can configure `tldrAI` using the `tldr_config()` function:

```r
tldr_config(
  # API Keys
  api_key = "your_claude_api_key",
  openai_api_key = "your_openai_api_key",
  
  # Provider settings
  provider = "claude",                # Default provider: "claude" or "openai"
  model = "claude-3-opus-20240229",   # Claude model
  openai_model = "gpt-4-turbo",       # OpenAI model
  
  # Fallback mechanism
  enable_fallback = TRUE,             # Enable fallback to another provider
  fallback_provider = "openai",       # Provider to use as fallback
  
  # Cache settings
  cache_enabled = TRUE,               # Enable/disable response caching
  cache_dir = "custom/cache/path",    # Custom cache directory
  cache_ttl = 30,                     # Cache time-to-live in days
  offline_mode = FALSE,               # Operate in offline mode (cache only)
  
  # Usage settings
  verbose_default = FALSE,            # Default verbosity setting
  examples_default = 2                # Default number of examples
)
```

## Offline Usage

`tldrAI` can operate in offline mode, using only cached responses:

```r
# Enable offline mode
tldr_offline(TRUE)

# Now all tldr() calls will use cached responses only
tldr("mean")

# Disable offline mode when you're back online
tldr_offline(FALSE)
```

## License

MIT