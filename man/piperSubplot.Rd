\name{piperSubplot}
\alias{piperSubplot}

\title{
Piper Diagram
}
\description{
Plot the Piper Diagram projected from the two trilinear diagrams on either side (support function).
}
\usage{
piperSubplot(x, y, what = "points", symbol = rep(1, length(x)), color = rep(1, length(x)), 
size = rep(0.05, length(x)), axis.range = c(0, 100), num.labels = 6, ticks = FALSE, 
grids = !ticks, x1title = "x1", y1title = "y1", x2title = "x2", y2title = "y2", plot = TRUE)
}

\arguments{
  \item{x}{
x-axis coordinate values (derived from the cation z-axis).
}
  \item{y}{
y-axis coordinate values (derived from the anion y-axis).
}
  \item{what}{
the type of plot, must be either "points" or "lines."
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
  \item{x1title}{
the title of the bottom x-axis.
}
  \item{y1title}{
the title of the left y-axis.
}
  \item{x2title}{
the title of the top x-axis.
}
  \item{y2title}{
the title of the right y-axis.
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
the transformed x- and y-coordinate values.
}
\seealso{
\code{\link{piperPlot}}
}
\keyword{ aplot }

