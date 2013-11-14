\name{hull}
\alias{hull}

\title{
Construct a Hull
}
\description{
Construct an enclosing hull from x- and y-coordinate data.
}
\usage{
hull(x, y, percent = 100, smooth = FALSE)
}
\arguments{
  \item{x}{
the x-coordinate data. Missing values are permitted, but ignored.
}
  \item{y}{
the y-coordinate data. Missing values are permitted, but ignored.
}
  \item{percent}{
the minimum percent to enclose.
}
  \item{smooth}{
smooth the bounding hull?
}
}
\value{
A list containing the x- and y-coordinates of the hull.
}
\seealso{
\code{\link{dataEllipse}}
}
\keyword{ dplot }
