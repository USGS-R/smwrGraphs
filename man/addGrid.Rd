\name{addGrid}
\alias{addGrid}
\title{
Grid Lines
}
\description{
Add grid lines to a graph.
}
\usage{
addGrid(current, Xgrid = list(grid = "gray50", finegrid = "none"), 
 Ygrid = list(grid = "gray50", finegrid = "none"))
}
\arguments{
  \item{current}{
the current plot information. Typically, this would be the output from one of the graph creation functions like \code{xyPlot}.
}
  \item{Xgrid}{
parameters defining the characteristics of the x-axis grid lines.
}
  \item{Ygrid}{
parameters defining the characteristics of the y-axis grid lines.
}
}
\details{
Information about grid lines is containined in the information returned from
high-level plotting functions in the USGSwsGraphs package.
}
\value{
NULL is returned invisibly.
}
\note{
The function \code{addGrid} should be used after setting up a graph with a high-level
plotting function in the USGSwsGraphs package and setting the \code{what} component in 
the \code{Plot} argument to "none." The graph can be completed by using \code{addXY}.
}

\seealso{
\code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{addXY}}, \code{\link{xyPlot}}
}
\keyword{aplot}

