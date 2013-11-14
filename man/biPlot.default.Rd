\name{biPlot.default}
\alias{biPlot.default}
\alias{biPlot}
\title{
Biplot
}
\description{
Produce a biplot, which is a plot of two different types of data on the same graph.
}
\usage{
biPlot(x, ...)
\method{biPlot}{default}(x, y, separate.axes = TRUE, 
xPlot = list(name ="observations", what = "points", type = "solid", 
 width = "standard", symbol = "circle", filled = TRUE, size = 0.05, color = "black"), 
yPlot = list(name = "variables", width = "color", size = 0.2, 
 color = "darkblue", symbol = "arrow", filled = FALSE), 
xPlotLabels = list(labels = "rownames", dir = "NE", size = 8, offset = 0.35), 
yPlotLabels = list(labels = "colnames", dir = "Auto", size = 8, offset = 0.35, 
color = "darkblue"), ylabels = 5, xlabels = 5, ylabels2 = 5, xlabels2 = 5,
xtitle = "", ytitle = "", xtitle2 = "", ytitle2 = "", caption = "",
margin = c(NA, NA, NA, NA), ...)
}

\arguments{
  \item{x}{
a matrix of x- and y-coordinates for observations or equivalent.
}
  \item{y}{
a matrix of x- and y-coordinates for variables or equivalent.
}
  \item{separate.axes}{
logical: should \code{x} and \code{y} data be plotted on separate axes?
}
  \item{xPlot}{
control information to plot the \code{x} data.
}
  \item{yPlot}{
control information to plot the \code{y} data.
}
  \item{xPlotLabels}{
control information for the \code{x} data labels.
}
  \item{yPlotLabels}{
control information for the \code{y} data labels.
}
  \item{ylabels}{
set y-axis labels for \code{x} data. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set x-axis labels for \code{x} data. See \code{\link{linearPretty}} for details.
}
  \item{ylabels2}{
set y-axis labels for \code{y} data. See \code{\link{linearPretty}} for details.
}
  \item{xlabels2}{
set x-axis labels for \code{y} data. See \code{\link{linearPretty}} for details.
}
  \item{xtitle}{
x-axis title for \code{x} data.
}
  \item{ytitle}{
y-axis title for \code{x} data.
}
  \item{xtitle2}{
x-axis title for \code{y} data.
}
  \item{ytitle2}{
y-axis title for \code{y} data.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins.
}
  \item{\dots}{
additional arguments for other methods.
}
}

\value{
Information about the graph
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{biPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{biPlot.princomp}}
}

\keyword{ hplot }

