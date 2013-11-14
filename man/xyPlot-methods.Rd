\name{xyPlot-methods}
\docType{methods}
\alias{xyPlot-methods}
\alias{xyPlot,numeric,numeric-method}
\alias{xyPlot,factor,numeric-method}
\title{Methods for Function \code{xyPlot}}
\description{
Create a line or scatter plot.
}
\usage{
\S4method{xyPlot}{numeric,numeric}(x, y, 
Plot = list(name = "", what = "points", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.log = FALSE, xaxis.range = c(NA, NA), ylabels = 7, xlabels = 7, 
xtitle = deparse(substitute(x)), ytitle = deparse(substitute(y)), 
caption = "", margin = c(NA, NA, NA, NA), ...)
\S4method{xyPlot}{factor,numeric}(x, y, 
Plot = list(name = "", what = "points", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.log = FALSE, xaxis.range = c(NA, NA), ylabels = 7, xlabels = "Auto", 
xtitle = "", ytitle = deparse(substitute(y)), 
caption = "", margin = c(NA, NA, NA, NA), xlabels.rotate = FALSE, ...)
}

\arguments{
  \item{x}{
the x-axis data
}
  \item{y}{
the y-axis data to plot
}
  \item{Plot}{
control parameters of the plot.
}
  \item{yaxis.log}{
log-transform the y axis?
}
  \item{yaxis.rev}{
reverse the y axis?
}
  \item{yaxis.range}{
set the range of the y-axis.
}
  \item{xaxis.log}{
log-transform the x axis?
}
  \item{xaxis.range}{
set the range of the x-axis.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set up x-axis labels. See \code{\link{linearPretty}} for details.
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
  \item{xlabels.rotate}{
rotate the x-axis labels 90 degrees?
}
  \item{\dots}{
additional arguments for other methods.
}
}
\details{
Setting \code{ylabels} or \code{xlabels} to 0 will suppress ticks and labels. That can be useful for relative
axes or specialized labeling. This feature only applies to \code{xyPlot}, setting \code{ylabels} or \code{xlabels} 
to 0 in any other high-level plotting function will cause an error.
}
\value{
Information about the graph
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{xyPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{timePlot}}, \code{\link{colorPlot}}
}
\section{Methods}{
\describe{
\item{\code{signature(x = "numeric", y = "numeric")}}{
Create a line or scatter plot from numeric x- and y-coordinate data.
}
\item{\code{signature(x = "factor", y = "numeric")}}{
Create a scatter plot or dot plot from factor x- and numeric y-coordinate data.
Most useful for setting up a bar chart.
}
}}
\keyword{methods}
\keyword{hplot}
