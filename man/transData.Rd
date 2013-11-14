\name{transData}
\alias{transData}
\title{
Transform Data
}
\description{
Transform numeric data to match any axis transform (support function).
}
\usage{
transData(data, logT = FALSE, revT = FALSE, trans = as.vector, transarg = NULL)
}
\arguments{
  \item{data}{
data for axis
}
  \item{logT}{
log transform?
}
  \item{revT}{
reverse data?
}
  \item{trans}{
arbitrary transform function.
}
  \item{transarg}{
list of arguments to \code{trans}.
}
}
\value{
A vector like \code{data} transformed to plot correctly on an axis.
}
\seealso{
\code{\link{transPlot}}, \code{\link{probPlot}}
}
\keyword{ dplot }
