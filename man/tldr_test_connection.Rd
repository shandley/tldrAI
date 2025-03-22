\name{tldr_test_connection}
\alias{tldr_test_connection}
\title{Test provider connection}
\usage{
tldr_test_connection(provider = NULL)
}
\arguments{
\item{provider}{The provider to test ("claude" or "openai")}
}
\value{
Logical indicating whether the connection was successful
}
\description{
Test the connection to an LLM provider's API to verify that your API key is valid
and that you have internet connectivity. This is useful for troubleshooting configuration
issues.
}
\examples{
\dontrun{
# Test connection to the default provider
tldr_test_connection()

# Test connection to a specific provider
tldr_test_connection("claude")
tldr_test_connection("openai")
}
}