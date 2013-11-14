\name{dotPlot-methods}
\docType{methods}
\alias{dotPlot-methods}
\alias{dotPlot,numeric-method}
\alias{dotPlot,Date-method}
\title{Methods for Function \code{dotPlot}}
\description{
Construct a dot plot.
}
\usage{
\S4method{dotPlot}{numeric}(x, y, 
Plot = list(name = "", what = "points", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.orient = "table", yaxis.order = "none", yaxis.grid = TRUE, xaxis.log = FALSE, 
xaxis.range = c(NA, NA), ylabels = "full", xlabels = 7, xtitle = deparse(substitute(x)), 
ytitle = "", caption = "", margin = c(NA, NA, NA, NA), jitter.y = TRUE, ...)
\S4method{dotPlot}{Date}(x, y, 
Plot = list(name = "", what = "points", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.orient = "table", yaxis.order = "none", yaxis.grid = TRUE, xaxis.log = FALSE, 
xaxis.range = range(x, na.rm=TRUE), ylabels = "full", xlabels = "Auto", xtitle = "", 
ytitle = "", caption = "", margin = c(NA, NA, NA, NA), jitter.y = TRUE, ...)
}

\arguments{
  \item{x}{
the x-axis data
}
  \item{y}{
the y-axis data, expected to be be either character or factor.
}
  \item{Plot}{
control parameters of the plot, see \code{link{setMultiPlot}} and \bold{Details} for details.
}
  \item{yaxis.orient}{
orientation of the y-axis values, must be either "table" or "grid." "Table" is sorted from top to bottom,
"grid" is sorted from bottom to top.
}
  \item{yaxis.order}{
the order of the y-axis values, must be one of "none," "ascending," or "descending."
}
  \item{yaxis.grid}{
draw grid lines?
}
  \item{xaxis.log}{
log-transform the x axis?
}
  \item{xaxis.range}{
set the range of the x-axis
}
  \item{ylabels}{
set up y-axis labels.
}
  \item{xlabels}{
set up x-axis labels.
}
  \item{xtitle}{
x-axis title.
}
  \item{ytitle}{
y-axis title.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
the parameters of the margin.
}
  \item{\dots}{
additional arguments for future methods.
}
  \item{jitter.y}{
adjust yvalues to reduce overlap for each group?
}
}
\details{
The \code{what} component of the \code{Plot} argument must be either "points" or "none."
}
\value{
Information about the graph.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{dotPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{boxPlot}}
}
\section{Methods}{
\describe{

\item{\code{signature(x = "numeric")}}{
Create a dot plot for numeric x-coordinate data and any (discrete) y-coordinate data.
}
}}
\keyword{methods}
\keyword{hplot}
