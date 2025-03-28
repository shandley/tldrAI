% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyring.R
\name{tldr_set_api_key}
\alias{tldr_set_api_key}
\title{Store API key securely in the system keyring}
\usage{
tldr_set_api_key(
  api_key,
  provider = c("claude", "openai"),
  update_config = TRUE
)
}
\arguments{
\item{api_key}{Character string containing the API key to store.}

\item{provider}{Character string specifying the provider ("claude" or "openai").}

\item{update_config}{Logical indicating whether to update the configuration.
When TRUE, removes the key from the config file after storing it securely.
Default is TRUE.}
}
\value{
Invisibly returns TRUE if the key was successfully stored.
}
\description{
This function securely stores an API key in the system's credential store
using the keyring package. The key is stored with a service name specific
to tldrAI and a username matching the provider name.
}
\section{Security Benefits}{

Using the system keyring provides several security advantages:
\itemize{
\item Keys are not stored in plain text in configuration files
\item System keyring is typically encrypted
\item Access to stored keys requires system authentication
\item Keys are not visible in R environment or history
}
}

\section{Fallback Behavior}{

If the keyring package is not available or fails:
\itemize{
\item A warning is displayed
\item The key is stored in the regular configuration (if update_config=TRUE)
\item The function returns FALSE
}
}

\examples{
\dontrun{
# Store Claude API key
tldr_set_api_key("your_claude_api_key", "claude")

# Store OpenAI API key
tldr_set_api_key("your_openai_api_key", "openai")

# Store key without updating configuration
tldr_set_api_key("your_api_key", "claude", update_config = FALSE)
}
}
