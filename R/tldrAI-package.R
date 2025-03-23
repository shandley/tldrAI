#' tldrAI: AI-Powered Quick Reference for R Functions
#'
#' @description
#' Provides concise, practical help for R functions using Large Language Models
#' while maintaining the brevity and example-focused philosophy of tldr.
#' 'tldrAI' connects to AI models (like Claude, OpenAI) to generate helpful,
#' practical examples and explanations for R functions without the verbosity
#' of traditional documentation.
#'
#' @section Key Features:
#' \itemize{
#'   \item \strong{Concise Help}: Get straight-to-the-point explanations
#'   \item \strong{Examples-First}: Prioritizes realistic code examples
#'   \item \strong{Multiple LLM Providers}: Support for both Claude and OpenAI APIs
#'   \item \strong{Contextual Awareness}: Provides personalized help based on your data
#'   \item \strong{Character Voices}: Add personality to responses
#'   \item \strong{Visualizations}: Generate function diagrams and code highlights
#'   \item \strong{Offline Mode}: Use cached responses when offline
#'   \item \strong{Asynchronous Requests}: Make non-blocking API calls
#' }
#'
#' @section Getting Started:
#' To use tldrAI, you need to configure at least one API key:
#'
#' \preformatted{
#' library(tldrAI)
#' # Secure storage (recommended)
#' tldr_set_api_key("your_claude_api_key", "claude")  # For Claude
#' tldr_set_api_key("your_openai_api_key", "openai")  # For OpenAI
#'
#' # Or using configuration file
#' tldr_config(api_key = "your_claude_api_key")  # For Claude
#' tldr_config(openai_api_key = "your_openai_api_key")  # For OpenAI
#' }
#'
#' Then you can get help for any R function:
#'
#' \preformatted{
#' tldr("mean")
#' tldr("dplyr::filter", context = TRUE)  # With contextual awareness
#' tldr("ggplot", visualize = TRUE)  # With visualization
#' }
#'
#' @section Secure API Key Management:
#' tldrAI offers secure API key storage using your system's credential store:
#'
#' \itemize{
#'   \item \strong{Secure Storage}: Store API keys in your system's secure keyring
#'   \item \strong{Automatic Migration}: Easily move existing keys to secure storage
#'   \item \strong{Flexible Fallbacks}: Use environment variables in CI/CD environments
#' }
#'
#' To store your API keys securely:
#'
#' \preformatted{
#' # Store API keys securely
#' tldr_set_api_key("your_claude_api_key", "claude")
#' tldr_set_api_key("your_openai_api_key", "openai")
#'
#' # Check if keys are available
#' tldr_has_api_key("claude")
#'
#' # Remove a key when no longer needed
#' tldr_clear_api_key("openai")
#'
#' # Migrate existing keys to keyring
#' tldr_key_migrate()
#' }
#'
#' @seealso
#' \code{\link{tldr}} for getting AI-powered help
#' \code{\link{tldr_config}} for configuration options
#' \code{\link{tldr_context_config}} for contextual awareness settings
#' \code{\link{tldr_set_api_key}} for secure API key storage
#' \code{\link{tldr_key_migrate}} for migrating keys to secure storage
#'
#' @docType package
#' @name tldrAI
NULL