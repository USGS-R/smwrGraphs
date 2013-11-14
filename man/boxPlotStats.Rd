\name{boxPlotStats}
\alias{boxPlotStats}
\title{
Compute Statistcs for a Box Plot
}
\description{
Comnpute the statistics for truncated, simple, or Tukey box plots.
}
\usage{
boxPlotStats(x, Box, yaxis.log)
}
\arguments{
  \item{x}{
a list containing the data to compute the statistics.
}
  \item{Box}{
control paramters for the box.
}
  \item{yaxis.log}{
logical: log transform data before computing the statistics?
}
}
\value{
a list containing the statistics for each boxplot.
}
\seealso{
\code{\link{boxPlot}}
}
\keyword{dplot}
