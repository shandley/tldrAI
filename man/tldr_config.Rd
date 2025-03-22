\name{tldr_config}
\alias{tldr_config}
\title{Configure tldrAI settings}
\usage{
tldr_config(
  api_key = NULL,
  openai_api_key = NULL,
  provider = NULL,
  model = NULL,
  openai_model = NULL,
  cache_enabled = NULL,
  cache_dir = NULL,
  cache_ttl = NULL,
  offline_mode = NULL,
  enable_fallback = NULL,
  fallback_provider = NULL,
  verbose_default = NULL,
  examples_default = NULL,
  character_voice = NULL,
  show_progress = NULL,
  debug_mode = NULL,
  async_mode = NULL,
  timeout = NULL,
  max_retries = NULL
)
}
\arguments{
\item{api_key}{Character string containing the API key for Claude}

\item{openai_api_key}{Character string containing the API key for OpenAI}

\item{provider}{Character string specifying the default provider ("claude" or "openai")}

\item{model}{Character string specifying which Claude model to use}

\item{openai_model}{Character string specifying which OpenAI model to use}

\item{cache_enabled}{Logical indicating whether to cache responses}

\item{cache_dir}{Character string specifying the cache directory}

\item{cache_ttl}{Numeric value indicating cache time-to-live in days}

\item{offline_mode}{Logical indicating whether to operate in offline mode (use cache only)}

\item{enable_fallback}{Logical indicating whether to enable provider fallback}

\item{fallback_provider}{Character string specifying the fallback provider}

\item{verbose_default}{Logical indicating the default verbosity}

\item{examples_default}{Integer indicating the default number of examples}

\item{character_voice}{Character string specifying the default character voice}

\item{show_progress}{Logical indicating whether to show a progress bar during API calls}

\item{debug_mode}{Logical indicating whether to show debug information}

\item{async_mode}{Logical indicating whether to use asynchronous API calls}

\item{timeout}{Numeric value specifying the API request timeout in seconds}

\item{max_retries}{Numeric value specifying the maximum number of retries for API calls}
}
\value{
Invisibly returns the updated configuration
}
\description{
Configure settings for the tldrAI package including API keys, providers, cache behavior,
performance options, and UI preferences.
}
\examples{
\dontrun{
tldr_config(api_key = "your_claude_api_key")
tldr_config(openai_api_key = "your_openai_api_key", provider = "openai")
tldr_config(enable_fallback = TRUE, fallback_provider = "openai")
tldr_config(verbose_default = TRUE)
tldr_config(offline_mode = TRUE)  # Use cached responses only
tldr_config(character_voice = "enthusiastic_explorer")  # Set default character voice
tldr_config(show_progress = TRUE)  # Show progress bar during API calls
tldr_config(async_mode = TRUE)  # Enable asynchronous API calls
tldr_config(timeout = 30)  # Set API request timeout to 30 seconds
tldr_config(max_retries = 5)  # Set maximum number of retries to 5
}
}