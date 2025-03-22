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

# Use a character voice for more personality
tldr("mean", voice = "enthusiastic_explorer")
tldr("sd", voice = "cynical_detective")

# See available character voices
tldr_list_voices()

# Test how a specific voice sounds
tldr_test_voice("eccentric_scientist")

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
- **Character Voices**: Add personality to responses with customizable character voices
- **Multiple LLM Providers**: Support for both Claude and OpenAI APIs
- **Provider Fallback**: Automatically switch to a fallback provider if the primary one fails
- **Progress Bar**: Visual feedback while waiting for API responses
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
  examples_default = 2,               # Default number of examples
  character_voice = "wise_mentor",    # Default character voice
  
  # UI settings
  show_progress = TRUE                # Show progress bar during API calls
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

## Character Voices

`tldrAI` includes several character voices that add personality to the function help:

```r
# List all available voices
tldr_list_voices()

# Try the original voices
tldr("mean", voice = "enthusiastic_explorer")  # Excited and energetic
tldr("sd", voice = "cynical_detective")        # Skeptical and direct
tldr("median", voice = "wise_mentor")          # Thoughtful and patient
tldr("ggplot", voice = "eccentric_scientist")  # Quirky and unpredictable

# Try the zany voices
tldr("mean", voice = "conspiracy_theorist")    # Paranoid but accurate
tldr("sd", voice = "exaggerator")              # Dramatically overstates everything
tldr("var", voice = "reluctant_helper")        # Comically put-upon
tldr("sum", voice = "time_traveler")           # From the future
tldr("matrix", voice = "theatrical_villain")   # Dramatically evil

# Set a default voice for all responses
tldr_config(character_voice = "wise_mentor")

# Test a voice with a standard function
tldr_test_voice("theatrical_villain")
```

Character voices transform the standard help text with personality traits while preserving all the important information. They modify section headings, add character-specific phrases, and adjust the tone of the response.

### Available Character Voices

| Voice Name | Description |
|------------|-------------|
| `none` | Plain responses with no personality |
| `enthusiastic_explorer` | Excited, curious, and eager to share discoveries |
| `cynical_detective` | Skeptical, direct, and cuts through the nonsense |
| `wise_mentor` | Thoughtful, patient, and shares wisdom with deep understanding |
| `eccentric_scientist` | Brilliantly chaotic, with quirky insights and unpredictable tangents |
| `conspiracy_theorist` | Provides accurate technical information but constantly relates it to far-fetched conspiracies |
| `exaggerator` | Dramatically overstates the importance and impact of every solution |
| `reluctant_helper` | Acts comically put-upon about having to provide assistance |
| `time_traveler` | Claims to be from the future and frames all solutions as "ancient techniques" |
| `theatrical_villain` | Presents solutions with dramatic flair as if revealing an evil master plan |

## License

MIT