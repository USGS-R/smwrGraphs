\name{USGS.colors}
\alias{blueRed.colors}
\alias{redBlue.colors}
\alias{coolWarm.colors}
\alias{warmCool.colors}
\alias{greenRed.colors}
\alias{redGreen.colors}
\alias{pastel.colors}
\title{
Generate a Range of Colors
}
\description{
Generates a sequence of colors along a specified range. 
}
\usage{
blueRed.colors(n, alpha = 1)
redBlue.colors(n, alpha = 1)
coolWarm.colors(n, alpha = 1)
warmCool.colors(n, alpha = 1)
greenRed.colors(n, alpha = 1)
redGreen.colors(n, alpha = 1)
pastel.colors(n, alpha = 1)
}

\arguments{
  \item{n}{
the number of colors to generate.
}
  \item{alpha}{
a measure of the intensity of the generated colors
}
}
\value{
A sequence of character strings indicating the colors.
}
\note{
\code{blueRed.colors} generates a sequence from blue to red through magenta.
\code{redBlue.colors} generates a sequence from red to blue through magenta.
\code{coolWarm.colors} generates a sequence from blue to red through green.
\code{warmCool.colors} generates a sequence from red to blue through green.
\code{greenRed.colors} generates a sequence from green to red through yellow.
\code{redGreen.colors} generates a sequence from red to green through yellow.
\code{pastel.colors} genereates a sequence of well-separated pastel colors useful for areas or bars.
}
\seealso{
\code{\link{rainbow}}, \code{\link{hcl}}
}
\keyword{color}

