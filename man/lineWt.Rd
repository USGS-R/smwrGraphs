\name{lineWt}
\alias{lineWt}
\alias{stdWt}
\alias{frameWt}
\title{
Line Weights
}
\description{
Compute the weight, or width, of a line
}
\usage{
lineWt(x)
stdWt(x = 1)
frameWt()
}
\arguments{
  \item{x}{
for \code{lineWt}, one of "standard," "color," "bold," or "hairline." For \code{stdWt},
a multiplier to set the line weight.
}
}
\value{
The width of the line.
}
\seealso{
\code{\link{par}}
}
\keyword{ dplot }

