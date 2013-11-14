\name{interpLine}
\alias{interpLine}
\title{
Interpolation
}
\description{
Create a vector of interpolated points along a line drawn by one of the \code{USGSwsGraphs} functions.
}
\usage{
interpLine(object, xfromy, yfromx, warn = TRUE, ...)
}
\arguments{
  \item{object}{
an object created by one of the \code{USGSwsGraphs} functions.
}
  \item{xfromy}{
the y-axis coordinate values to use to create matching x-coordinate values.
Missing values are permitted but result in missing values in the output.
}
  \item{yfromx}{
the x-axis coordinate values to use to create matching y-coordinate values.
Missing values are permitted but result in missing values in the output.
}
  \item{warn}{
supress the warning message from probability or transformed axes plots?
}
  \item{\dots}{
any future additional arguments.
}
}
\details{
Exactly one of \code{xfromy} or \code{yfromx} must be specified in the call.
}
\value{
A vector of numeric values corresponding to those values in either \code{xfromy} or \code{yfromx}.
}
\note{
The back-transformation information is not included in the output from the graphics functions. This
primarily affects the \code{transPlot} function.
}
\seealso{
\code{\link{transPlot}}
}
\examples{
## Note the function needs to be modified and probPlot may need to be modified to indicate that it
## has returned specialized x-axis data that do not need transformation. Need to work on a system.
## might be enough to check only that ytrans exists.
}
\keyword{dplot}

