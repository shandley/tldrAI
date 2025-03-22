\name{tldr_debug}
\alias{tldr_debug}
\title{Toggle debug mode}
\usage{
tldr_debug(enable = TRUE)
}
\arguments{
\item{enable}{Logical indicating whether to enable (TRUE) or disable (FALSE) debug mode}
}
\value{
Invisibly returns the updated configuration
}
\description{
Toggle debug mode to show additional diagnostic information. When debug mode is enabled,
tldrAI will display detailed information about function lookups, API calls, cache usage,
and other internal processes.
}
\examples{
\dontrun{
# Enable debug mode
tldr_debug(TRUE)

# Look up a function with debug information displayed
tldr("mean")

# Disable debug mode
tldr_debug(FALSE)
}
}