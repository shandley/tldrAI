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
#' @seealso
#' \code{\link{tldr}} for getting AI-powered help
#' \code{\link{tldr_config}} for configuration options
#' \code{\link{tldr_context_config}} for contextual awareness settings
#'
#' @docType package
#' @name tldrAI
NULL