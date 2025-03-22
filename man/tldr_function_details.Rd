\name{tldr_function_details}
\alias{tldr_function_details}
\title{Get detailed function information}
\usage{
tldr_function_details(func_name, pkg = NULL)
}
\arguments{
\item{func_name}{Character string specifying the name of an R function}

\item{pkg}{Character string specifying the package name (optional)}
}
\value{
A list with detailed function information
}
\description{
Extracts detailed information about an R function including its arguments, 
package, description, and usage. This is a lower-level function that provides
raw data about a function without AI-generated content.
}
\examples{
\dontrun{
# Get details for base R function
details <- tldr_function_details("mean")
str(details)

# Get details for a function in a specific package
details <- tldr_function_details("filter", "dplyr")

# Alternative syntax using package::function
details <- tldr_function_details("dplyr::filter")
}
}