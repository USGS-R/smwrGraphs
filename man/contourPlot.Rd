\name{contourPlot}
\alias{contourPlot}
\alias{contourPlot.default}
\alias{contourPlot.matrix}
\title{
Contour Plot
}
\description{
Produce a contour plot or filled color map to show three-dimensional data.
}
\usage{
contourPlot(z, ...)
\method{contourPlot}{default}(z, x, y, Grid = list(method = "interpolate", linear = TRUE, 
extrapolate = FALSE, density = 90, span = 0.75, degree = 1, family = "symmetric"), 
Contours = list(name = "Auto", levels = 10, filled = FALSE, lineColor = "black", 
lineLabel = "flattest", fillColors = "coolWarm"), yaxis.range = c(NA, NA), xaxis.range = c(NA, NA), 
ylabels = 4, xlabels = 4, xtitle = deparse(substitute(x)), ytitle = deparse(substitute(y)), 
caption = "", margin = c(NA, NA, NA, NA), ...)
\method{contourPlot}{matrix}(z, rows, cols, matrix.rows="x", Contours = list(name = "Auto", 
levels = 10, filled = FALSE, lineColor = "black", lineLabel = "flattest", fillColors = "coolWarm"), 
yaxis.range = c(NA, NA), xaxis.range = c(NA, NA), ylabels = 4, xlabels = 4, 
xtitle = deparse(substitute(x)), ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), ...)
}
\arguments{
  \item{z}{
the values to be contoured. 
}
  \item{x}{
the x-axis coordinates for \code{z}.
}
  \item{y}{
the y-axis coordinates for \code{z}.
}
  \item{rows}{
the coordinates for \code{z} represented by the rows in the matrix.
}
  \item{cols}{
the coordinates for \code{z} represented by the columns in the matrix.
}
  \item{matrix.rows}{
a single character, either "x" or "y" indicating whether the rows in \code{z} should be
plotted along the x or y axis.
}
  \item{Grid}{
control paramters for gridding irregularly spaced data.
}
  \item{Contours}{
control parameters for the coutour lines or levels in the filled plot.
}
  \item{yaxis.range}{
set the range of the y-axis.
}
  \item{xaxis.range}{
set the range of the x-axis.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set up x-axis labels. See \code{\link{linearPretty}} for details.~
}
  \item{xtitle}{
the x-axis title.
}
  \item{ytitle}{
the y-axis title.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins.
}
  \item{\dots}{
any additional arguments. Currently not used.
}
}
\details{
  Missing values are permitted in \code{x}, \code{y}, and \code{z} for
  the default method and removed before constructing a grid with a
  warning.\cr
  Missing values are not permitted in \code{x} or \code{y} for
  the matrix method, but are permitted for \code{z}, which results in
  blank areas in the graph.
}
\value{
Information about the graph.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{contourPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{contourLines}}
}
\keyword{hplot}
