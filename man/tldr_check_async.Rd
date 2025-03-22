\name{tldr_check_async}
\alias{tldr_check_async}
\title{Check and retrieve results from asynchronous tldr calls}
\usage{
tldr_check_async(wait = TRUE, timeout = 30)
}
\arguments{
\item{wait}{Logical indicating whether to wait for the result if not ready}

\item{timeout}{Numeric value specifying how long to wait for the result (in seconds)}
}
\value{
The result of the asynchronous tldr call
}
\description{
Check and retrieve results from asynchronous tldr calls
}
\examples{
\dontrun{
tldr("mean", async = TRUE)
# Do other work...
result <- tldr_check_async()
}
}