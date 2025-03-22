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
\examples{
\dontrun{
tldr_context_config(enable_context_awareness = TRUE)
tldr_context_config(analyze_data_frames = TRUE, anonymize_data = TRUE)
tldr_context_config(max_rows_sample = 3, max_cols_sample = 5)
tldr_context_config(analyze_history = FALSE)  # Disable command history analysis
}
}