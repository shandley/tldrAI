\name{tldr_test_context}
\alias{tldr_test_context}
\title{Test the context analyzer by showing what data it collects}
\usage{
tldr_test_context()
}
\value{
Invisibly returns the context data as a list object, which can be assigned to a variable
for inspection
}
\description{
Test the context analyzer by showing what data it collects from your R environment
}
\details{
This function runs the context analyzer and displays exactly what information is being
collected from your R environment when you use \code{context_aware = TRUE} with
\code{tldr()} or \code{tldr_explain()}. It helps you understand:

\itemize{
  \item What data frames are being analyzed
  \item How much of your data is being sampled
  \item What packages and functions are being detected
  \item What command history is being collected
  \item How your environment is being analyzed
}

This is particularly useful when:
\itemize{
  \item Debugging context-aware explanations
  \item Checking privacy implications of context awareness
  \item Understanding why certain context information appears in explanations
  \item Testing how code blocks interact with context awareness
}

For code blocks in \code{tldr_explain()}, the context analyzer will:
\enumerate{
  \item Extract variables and function calls from code
  \item Detect potential packages based on function patterns
  \item Match with objects in your environment
  \item Score relevance of data frames
  \item Format context information for explanations
}

The output shows you exactly what tldrAI "sees" when generating context-aware explanations.
}
\examples{
\dontrun{
# Basic test of context collection
tldr_test_context()

# Store the context data for inspection
context_data <- tldr_test_context()
str(context_data)  # Examine the structure

# Test with specific objects in environment
library(dplyr)
library(ggplot2)
data <- mtcars
filtered_data <- filter(data, mpg > 20)
plot <- ggplot(filtered_data, aes(wt, mpg)) + geom_point()
tldr_test_context()  # Now shows these objects

# Compare context with different privacy settings
tldr_context_config(anonymize_data = TRUE)
tldr_test_context()  # Data values are anonymized
tldr_context_config(anonymize_data = FALSE)
tldr_test_context()  # Actual data values are shown
}
}