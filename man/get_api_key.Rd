% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyring.R
\name{get_api_key}
\alias{get_api_key}
\title{Get API key securely}
\usage{
get_api_key(provider = c("claude", "openai"))
}
\arguments{
\item{provider}{Character string specifying the provider ("claude" or "openai").}
}
\value{
Character string containing the API key, or NULL if no key is found.
}
\description{
This function retrieves an API key for the specified provider, checking
the system keyring, environment variables, and configuration file in that order.
}
\keyword{internal}
