\name{strip.blanks}
\alias{strip.blanks}
\title{
Remove Spaces
}
\description{
Remove surrounding spaces from text.
}
\usage{
strip.blanks(x)
}

\arguments{
  \item{x}{
a character vector.
}
}
\value{
A vector like \code{x}, but with leading and trailing spaces removed from each element.
}
\seealso{
\code{link{sub}}
}
\examples{
strip.blanks("   keep me   ")
}
\keyword{manip}
