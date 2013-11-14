\name{dataEllipse}
\alias{dataEllipse}

\title{
Construct an Ellipse
}
\description{
Construct an ellipse from x- and y-coordinate data.
}
\usage{
dataEllipse(x, y, percent = 100, smooth = 0)
}

\arguments{
  \item{x}{
the x-coordinate data.
}
  \item{y}{
the y-coordinate data.
}
  \item{percent}{
a scale factor, adjusted to include \code{percent} of the data.
}
  \item{smooth}{
required for compatibiltiy, not used.
}
}
\value{
A list containing the x- and y-coordinates of the ellipse.
}
\seealso{
\code{\link{cov2Ellipse}}, \code{\link{hull}}
}
\keyword{ dplot }
