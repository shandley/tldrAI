\name{tldr}
\alias{tldr}
\title{Get AI-powered quick reference for R functions}
\usage{
tldr(
  func_name,
  verbose = NULL,
  examples = NULL,
  refresh = FALSE,
  provider = NULL,
  voice = NULL,
  async = NULL,
  context = NULL
)
}
\arguments{
\item{func_name}{Character string specifying the name of an R function}

\item{verbose}{Logical indicating whether to include more detailed information}

\item{examples}{Integer indicating the number of examples to include}

\item{refresh}{Logical indicating whether to ignore cached results}

\item{provider}{Character string specifying the LLM provider to use ("claude" or "openai")}

\item{voice}{Character string specifying the character voice to use (e.g., "enthusiastic_explorer")}

\item{async}{Logical indicating whether to make the API call asynchronously}

\item{context}{Logical indicating whether to use contextual awareness to personalize examples}
}
\value{
Prints formatted help to the console (invisibly returns the raw response)
}
\description{
Get AI-powered quick reference for R functions
}
\examples{
\dontrun{
tldr("mean")
tldr("ggplot", examples = 3)
tldr("dplyr::filter", provider = "openai")
tldr("median", voice = "enthusiastic_explorer")
tldr("sd", voice = "cynical_detective")
tldr("plot", async = TRUE)  # Make an asynchronous API call
tldr("filter", context = TRUE)  # Use contextual awareness
}
}