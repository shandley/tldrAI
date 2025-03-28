% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{create_visualization}
\alias{create_visualization}
\title{Create a visualization for a function}
\usage{
create_visualization(
  func_name,
  metadata,
  vis_type = "diagram",
  prompt_install = TRUE
)
}
\arguments{
\item{func_name}{Character string specifying the name of the function}

\item{metadata}{List with function metadata}

\item{vis_type}{Character string specifying the visualization type ("diagram" or "flowchart")}

\item{prompt_install}{Logical indicating whether to prompt for installation if packages are missing}
}
\value{
A visualization object or NULL if not available
}
\description{
Create a visualization for a function
}
\examples{
\dontrun{
metadata <- get_function_metadata("mean")
vis <- create_visualization("mean", metadata)
print(vis)

# Without installation prompt
vis <- create_visualization("mean", metadata, prompt_install = FALSE)
}
}
