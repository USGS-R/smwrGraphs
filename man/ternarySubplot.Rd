\name{ternarySubplot}
\alias{ternarySubplot}
\title{
Piper Diagram
}
\description{
Produce a trilinear diagram for the Piper Diagram (support function).
}
\usage{
ternarySubplot(x, y, z, what = "points", symbol = rep(1, length(x)), color = rep(1, length(x)), 
size = rep(0.05, length(x)), axis.range = c(0, 100), num.labels = 6, ticks = FALSE, 
grids = !ticks, orient = "c", xtitle = "x", ytitle = "y", ztitle = "z", plot = TRUE)
}

\arguments{
  \item{x}{
the x-ccordinate values
}
  \item{y}{
the y-coordinate values
}
  \item{z}{
the z-coordinate values
}
  \item{what}{
the type of plot, must be either "points," "lines," or "none."
}
  \item{symbol}{
the symbol to use if \code{what} is "points."
}
  \item{color}{
the color of the plot.
}
  \item{size}{
the size of the symbol if \code{what} is "points."
}
  \item{axis.range}{
the range of the axes. Must be either c(0, 1) or c(0, 100).
}
  \item{num.labels}{
the number of labels to draw on each axis.
}
  \item{ticks}{
draw ticks?
}
  \item{grids}{
draw grid lines?
}
  \item{orient}{
a single character, "c" indicates clockwise orientation for \code{x}, \code{y}, and \code{z} anything else indicates counter-clockwise.
}
  \item{xtitle}{
the x-axis title.
}
  \item{ytitle}{
the y-axis title.
}
  \item{ztitle}{
the z-axis title.
}
  \item{plot}{
plot the data?
}
}
\details{
Support function, to be called only from \code{piperPlot}.
}
\value{
If \code{plot} is TRUE, then the range of the user coordinates. Otherwise,
the \code{x}, \code{y}, and \code{z} values are converted to 2-dimensional values. 
}
\seealso{
\code{\link{piperPlot}}
}
\keyword{ aplot }

