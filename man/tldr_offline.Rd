\name{tldr_offline}
\alias{tldr_offline}
\title{Toggle offline mode}
\usage{
tldr_offline(enable = TRUE)
}
\arguments{
\item{enable}{Logical indicating whether to enable (TRUE) or disable (FALSE) offline mode}
}
\value{
Invisibly returns the updated configuration
}
\description{
Toggle offline mode to use only cached responses when you don't have internet access.
When offline mode is enabled, tldrAI will only use cached responses and will not
make any API calls.
}
\examples{
\dontrun{
# Enable offline mode
tldr_offline(TRUE)

# Use functions from cache (no API calls)
tldr("mean")
tldr("median")

# Disable offline mode
tldr_offline(FALSE)
}
}