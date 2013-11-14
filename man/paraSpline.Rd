\name{paraSpline}
\alias{paraSpline}
\title{
Parametric Spline
}
\description{
Construct a parametric interpolating spline for x and y data for not necessarily
strictly increasing x. Used as a support function.
}
\usage{
paraSpline(x, y, n)
}
\arguments{
  \item{x}{
the x-coordinate data. Missing values are not permitted.
}
  \item{y}{
the y-coordinate data. Missing values are not permitted.
}
  \item{n}{
The number of points in the output parametric spline fit.
}
}
\value{
A list containing the components \code{x} and \code{y}, which are the 
coordinates of the parametric spline.
}
\keyword{dplot}

