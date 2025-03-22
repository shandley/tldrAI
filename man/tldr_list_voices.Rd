\name{tldr_list_voices}
\alias{tldr_list_voices}
\title{List available character voices}
\usage{
tldr_list_voices()
}
\value{
Prints available voices to the console (invisibly returns a character vector of voice names)
}
\description{
Lists all available character voices that can be used to add personality to the function 
documentation. Each voice has a unique style and tone while preserving the core technical
information.
}
\examples{
\dontrun{
# List all available character voices
tldr_list_voices()

# Use a specific voice
tldr("mean", voice = "enthusiastic_explorer")
}
}