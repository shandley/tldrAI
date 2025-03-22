# tldrAI Usage Guide

## Basic Usage

The `tldr()` function provides concise, AI-generated documentation for R functions.

```r
# Basic usage
tldr("mean")

# Get more detailed information
tldr("mean", verbose = TRUE)

# Show more examples
tldr("ggplot2::ggplot", examples = 3)

# Use a specific provider
tldr("dplyr::filter", provider = "openai")

# Add personality with a character voice
tldr("median", voice = "enthusiastic_explorer")

# Make an asynchronous API call
tldr("plot", async = TRUE)
```

## Asynchronous Usage

For better performance, you can make asynchronous API calls:

```r
# Start an async request
tldr("mean", async = TRUE)

# Do other work while the request is processing...

# Check and retrieve the result when ready
result <- tldr_check_async()

# Check without waiting (returns NULL if not ready)
tldr_check_async(wait = FALSE)

# Wait with a custom timeout (in seconds)
tldr_check_async(timeout = 60)
```

## Configuration

Configure the package with `tldr_config()`:

```r
# Set API keys
tldr_config(api_key = "your_claude_api_key")
tldr_config(openai_api_key = "your_openai_api_key")

# Set default provider
tldr_config(provider = "claude")  # or "openai"

# Configure performance settings
tldr_config(timeout = 60)        # API request timeout in seconds
tldr_config(max_retries = 3)     # Maximum number of retries
tldr_config(async_mode = TRUE)   # Enable async mode by default

# Configure caching
tldr_config(cache_enabled = TRUE)
tldr_config(cache_ttl = 30)      # Cache time-to-live in days

# Set UI preferences
tldr_config(verbose_default = FALSE)
tldr_config(examples_default = 2)
tldr_config(show_progress = TRUE)
tldr_config(character_voice = "wise_mentor")
```

## Offline Mode

Use cached responses when you're offline:

```r
# Enable offline mode
tldr_offline(TRUE)

# Disable offline mode
tldr_offline(FALSE)
```

## Cache Management

Manage the response cache:

```r
# Clear the entire cache
tldr_cache_clear()

# Clear only expired cache entries
tldr_cache_clear(expired_only = TRUE)
```

## Character Voices

Add personality to responses:

```r
# List all available voices
tldr_list_voices()

# Test a specific voice
tldr_test_voice("eccentric_scientist")

# Set a default voice
tldr_config(character_voice = "wise_mentor")
```

## Other Utilities

```r
# Test connection to a provider
tldr_test_connection("claude")

# Enable debug mode for troubleshooting
tldr_debug(TRUE)

# Open the GitHub repository in your browser
tldr_open_repo()
```