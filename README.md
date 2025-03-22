# tldrAI

> AI-Powered Quick Reference for R Functions

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Features](#features)
- [Usage](#usage)
- [Configuration](#configuration)
- [Offline Usage](#offline-usage)
- [Contextual Awareness](#contextual-awareness)
  - [Privacy and Customization](#privacy-and-customization)
  - [Testing Context Analysis](#testing-context-analysis)
- [Performance Features](#performance-features)
  - [Asynchronous API Calls](#asynchronous-api-calls)
  - [Performance Tuning](#performance-tuning)
- [Character Voices](#character-voices)
  - [Available Character Voices](#available-character-voices)
- [Support the Project](#support-the-project)
- [License](#license)

## Overview

`tldrAI` modernizes the concept of the tldr package with AI capabilities. It provides concise, practical help for R functions using LLMs while maintaining the brevity and example-focused philosophy of tldr.

## Installation

```r
# Install from GitHub
# devtools::install_github("username/tldrAI")
```

## Features

- **Concise Help**: Get straight-to-the-point explanations focused on practical usage
- **Examples-First**: Prioritizes realistic code examples over verbose documentation
- **Contextual Awareness**: Provides personalized help based on your actual data and code
- **Character Voices**: Add personality to responses with customizable character voices
- **Multiple LLM Providers**: Support for both Claude and OpenAI APIs
- **Provider Fallback**: Automatically switch to a fallback provider if the primary one fails
- **Progress Bar**: Visual feedback while waiting for API responses
- **Offline Mode**: Use cached responses when you're offline
- **Cache Management**: Intelligent caching with TTL and selective clearing
- **Customizable**: Configure verbosity, number of examples, and other preferences
- **Package Functions**: Support for functions from any package with `package::function` syntax
- **Asynchronous Requests**: Make non-blocking API calls to improve performance
- **Intelligent Retries**: Exponential backoff and smart retry logic for reliability
- **Performance Tuning**: Configurable timeouts and retry attempts
- **Privacy Controls**: Configure what data is shared with AI models

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
tldr("stats::sd", verbose = TRUE, examples = 3)

# Always use package::function notation for clarity with popular packages
tldr("ggplot2::ggplot", examples = 3)
tldr("dplyr::filter", provider = "openai")

# Use contextual awareness for personalized examples
tldr("filter", context = TRUE)
tldr("ggplot", context = TRUE)

# Use a character voice for more personality
tldr("mean", voice = "enthusiastic_explorer")
tldr("sd", voice = "cynical_detective")

# Combine contextual awareness with character voices
tldr("summarise", context = TRUE, voice = "eccentric_scientist")

# Configure contextual awareness settings
tldr_context_config(enable_context_awareness = TRUE)  # Enable by default
tldr_context_config(anonymize_data = TRUE)  # Protect sensitive data

# See what context data would be collected
tldr_test_context()

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

# Open the GitHub repository in your browser
tldr_open_repo()
```

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
  show_progress = TRUE,               # Show progress bar during API calls
  
  # Performance settings
  async_mode = FALSE,                 # Enable asynchronous API calls
  timeout = 60,                       # API request timeout in seconds
  max_retries = 3                     # Maximum number of retry attempts
)

# Configure contextual awareness separately
tldr_context_config(
  enable_context_awareness = TRUE,    # Enable contextual awareness by default
  analyze_data_frames = TRUE,         # Analyze data frames in the environment
  analyze_packages = TRUE,            # Analyze loaded packages
  analyze_history = TRUE,             # Analyze command history
  anonymize_data = TRUE,              # Don't include actual data samples
  max_rows_sample = 5,                # Maximum rows to sample
  max_cols_sample = 5,                # Maximum columns to sample
  include_row_count = TRUE,           # Include row counts in context
  include_class_info = TRUE,          # Include class information
  include_column_types = TRUE,        # Include column types in context
  max_history_commands = 10           # Maximum history commands to analyze
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

## Contextual Awareness

The contextual awareness feature helps `tldrAI` provide personalized help based on your actual R environment. When enabled, it analyzes your current data frames, loaded packages, and recent commands to tailor examples and explanations to your specific context.

```r
# Use contextual awareness for a single query
tldr("filter", context = TRUE)

# Enable contextual awareness for all queries by default
tldr_context_config(enable_context_awareness = TRUE)

# Now all tldr() calls will use contextual awareness
tldr("ggplot")  # Will provide examples using your actual data frames

# You can still override the default for a specific call
tldr("mean", context = FALSE)  # Disable context for this specific call
```

When contextual awareness is enabled, `tldrAI` will:

1. **Analyze your data frames** to understand their structure, column names, and data types
2. **Examine loaded packages** to provide examples that work with your current environment
3. **Review recent command history** to suggest logical next steps in your workflow
4. **Generate examples** using your actual data structures instead of generic examples
5. **Suggest workflow patterns** based on your usage patterns

### Privacy and Customization

You can customize what information is analyzed and included in prompts to the AI model:

```r
# Configure privacy settings
tldr_context_config(
  analyze_data_frames = TRUE,    # Whether to analyze data frames
  analyze_packages = TRUE,       # Whether to analyze loaded packages
  analyze_history = TRUE,        # Whether to analyze command history
  anonymize_data = TRUE,         # Whether to anonymize data (recommended)
  max_rows_sample = 3,           # Maximum rows to sample
  max_cols_sample = 5            # Maximum columns to sample
)
```

By default, data anonymization is enabled, which means:

- Actual data values from your data frames are never sent to the AI
- Only structural information like column names, data types, and dimensions are included
- Command history is filtered to remove potentially sensitive information

### Testing Context Analysis

You can see exactly what information would be collected and sent to the AI model:

```r
# View a summary of what context data would be collected
tldr_test_context()
```

This will display a report showing:

- What data frames were detected and their structure
- What packages are loaded
- What recent functions were used
- What environment information was captured

This is helpful for understanding what information shapes the contextual examples and for verifying that sensitive data is not being shared.

## Performance Features

### Asynchronous API Calls

`tldrAI` supports non-blocking asynchronous API calls to improve performance:

```r
# Make an asynchronous request
tldr("mean", async = TRUE)

# Continue working on other tasks...
# ...

# Check and retrieve the result when ready
result <- tldr_check_async()

# Check without waiting (returns NULL if not ready)
tldr_check_async(wait = FALSE)

# Wait with a custom timeout (in seconds)
tldr_check_async(timeout = 60)

# Enable async mode by default
tldr_config(async_mode = TRUE)
```

### Performance Tuning

You can optimize API call performance with these settings:

```r
# Adjust timeout for slow connections
tldr_config(timeout = 120)  # 2 minutes

# Increase retry attempts for unreliable networks
tldr_config(max_retries = 5)
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

## Support the Project

If you find tldrAI useful, please consider starring the repository on GitHub! This helps in several important ways:

- **Visibility**: Stars help other R users discover the package
- **Motivation**: It encourages continued development and maintenance
- **Feedback**: Stars indicate the package is valuable to the community
- **Networking**: It helps connect developers with similar interests

You can easily star the repository by using:

```r
# Open the GitHub repository in your browser
tldr_open_repo()
```

Then click the ⭐ (Star) button in the top-right corner of the repository page.

## License

MIT