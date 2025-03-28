% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyring.R
\name{tldr_has_api_key}
\alias{tldr_has_api_key}
\title{Check if API key is available}
\usage{
tldr_has_api_key(provider = c("claude", "openai"))
}
\arguments{
\item{provider}{Character string specifying the provider ("claude" or "openai").}
}
\value{
Logical indicating whether an API key is available.
}
\description{
This function checks if an API key is available for the specified provider,
either in the system keyring, environment variables, or configuration file.
}
\examples{
\dontrun{
# Check if Claude API key is available
tldr_has_api_key("claude")

# Check if OpenAI API key is available
tldr_has_api_key("openai")
}
}
