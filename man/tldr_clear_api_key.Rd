% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyring.R
\name{tldr_clear_api_key}
\alias{tldr_clear_api_key}
\title{Clear API key from system keyring}
\usage{
tldr_clear_api_key(provider = c("claude", "openai"), confirm = TRUE)
}
\arguments{
\item{provider}{Character string specifying the provider ("claude" or "openai").}

\item{confirm}{Logical indicating whether to ask for confirmation before clearing.
Default is TRUE.}
}
\value{
Invisibly returns TRUE if the key was successfully removed or FALSE if
the operation was cancelled or failed.
}
\description{
This function removes an API key from the system keyring for the specified provider.
}
\examples{
\dontrun{
# Clear Claude API key
tldr_clear_api_key("claude")

# Clear OpenAI API key without confirmation
tldr_clear_api_key("openai", confirm = FALSE)
}
}
