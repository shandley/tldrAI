\name{tldr_cache_clear}
\alias{tldr_cache_clear}
\title{Clear the cache}
\usage{
tldr_cache_clear(confirm = TRUE, expired_only = FALSE)
}
\arguments{
\item{confirm}{Logical indicating whether to ask for confirmation}

\item{expired_only}{Logical indicating whether to clear only expired cache entries}
}
\value{
Invisibly returns TRUE if cache was cleared
}
\description{
Clear cached function documentation responses. You can either clear all entries
or only expired ones. By default, the function asks for confirmation before deleting
the cache.
}
\examples{
\dontrun{
# Clear all cache entries (with confirmation)
tldr_cache_clear()

# Clear all cache entries without confirmation
tldr_cache_clear(confirm = FALSE)

# Clear only expired cache entries
tldr_cache_clear(expired_only = TRUE)
}
}