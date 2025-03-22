\name{tldr_test_voice}
\alias{tldr_test_voice}
\title{Test a character voice}
\usage{
tldr_test_voice(voice_name)
}
\arguments{
\item{voice_name}{Character string specifying the voice to test}
}
\value{
Prints an example of the specified voice (invisibly returns the transformed text)
}
\description{
Tests a specific character voice by applying it to a standard example function.
This allows you to see how the voice transforms documentation before using it
with actual function lookups.
}
\examples{
\dontrun{
# Test different character voices
tldr_test_voice("enthusiastic_explorer")
tldr_test_voice("cynical_detective")
tldr_test_voice("eccentric_scientist")
tldr_test_voice("time_traveler")
tldr_test_voice("theatrical_villain")
}
}