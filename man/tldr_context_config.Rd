\name{tldr_context_config}
\alias{tldr_context_config}
\title{Configure context awareness settings for tldrAI}
\usage{
tldr_context_config(
  enable_context_awareness = NULL,
  analyze_data_frames = NULL,
  analyze_packages = NULL,
  analyze_history = NULL,
  anonymize_data = NULL,
  max_rows_sample = NULL,
  max_cols_sample = NULL,
  include_row_count = NULL,
  include_class_info = NULL,
  include_column_types = NULL,
  max_history_commands = NULL
)
}
\arguments{
\item{enable_context_awareness}{Logical indicating whether to enable context awareness}

\item{analyze_data_frames}{Logical indicating whether to analyze data frames}

\item{analyze_packages}{Logical indicating whether to analyze active packages}

\item{analyze_history}{Logical indicating whether to analyze command history}

\item{anonymize_data}{Logical indicating whether to anonymize data samples}

\item{max_rows_sample}{Integer specifying maximum rows to sample from data frames}

\item{max_cols_sample}{Integer specifying maximum columns to sample from data frames}

\item{include_row_count}{Logical indicating whether to include row counts in context}

\item{include_class_info}{Logical indicating whether to include class information}

\item{include_column_types}{Logical indicating whether to include column types}

\item{max_history_commands}{Integer specifying maximum number of history commands to analyze}
}
\value{
Invisibly returns the updated configuration
}
\description{
Configure context awareness settings for tldrAI
}
\details{
Context awareness enhances explanations by analyzing your R environment. These settings
control what information is collected and how it's used:

\itemize{
  \item \strong{Data frames}: When \code{analyze_data_frames = TRUE}, tldrAI examines data frames
    in your environment and determines which are most relevant to your code or function.
    
  \item \strong{Variable relevance}: For \code{tldr_explain()}, variables in your code are matched
    with objects in your environment. For example, if your code references \code{diamonds_small}
    and that exists in your environment, tldrAI will use its properties in the explanation.
    
  \item \strong{Package detection}: When \code{analyze_packages = TRUE}, tldrAI identifies functions
    from packages like dplyr, ggplot2, and tidyr in your code and provides relevant contextual information.
    For code blocks, it automatically detects which packages are being used based on function patterns.
    
  \item \strong{Command history}: When \code{analyze_history = TRUE}, tldrAI analyzes recent commands
    to provide additional context about your workflow.
    
  \item \strong{Privacy settings}: Use \code{anonymize_data = TRUE} to replace actual data values with
    placeholders while preserving structure and types.
}

For \code{tldr_explain()} with code blocks, context awareness works by:
\enumerate{
  \item Extracting variables and function calls from your code
  \item Detecting potential packages based on function patterns
  \item Matching those with objects and packages in your environment
  \item Providing relevant contextual information in the explanation
  \item Suggesting relevant examples using your actual data
}

Use \code{\link{tldr_test_context}} to see what information is collected with your current settings.
}
\examples{
\dontrun{
# Enable all context awareness features
tldr_context_config(enable_context_awareness = TRUE)

# Configure data frame analysis with privacy protection
tldr_context_config(analyze_data_frames = TRUE, anonymize_data = TRUE)

# Limit data sampling for privacy or performance
tldr_context_config(max_rows_sample = 3, max_cols_sample = 5)

# Disable command history analysis
tldr_context_config(analyze_history = FALSE)

# Example workflow with context awareness for code blocks
library(dplyr)
library(ggplot2)
data <- mtcars
tldr_context_config(enable_context_awareness = TRUE)
code <- 'data %>% 
  filter(mpg > 20) %>%
  ggplot(aes(wt, mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Fuel Efficiency vs Weight by Cylinders")'
tldr_explain(code, context_aware = TRUE)
}
}