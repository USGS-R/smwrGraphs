\name{timePlot-methods}
\docType{methods}
\alias{timePlot-methods}
\alias{timePlot,Date,numeric-method}
\alias{timePlot,POSIXt,numeric-method}
\alias{timePlot,numeric,numeric-method}
\alias{timePlot,integer,numeric-method}
\alias{timePlot,difftime,numeric-method}
\title{Methods for Function \code{timePlot}}
\description{
Produce a plot of time-series data.
}
\usage{
\S4method{timePlot}{Date,numeric}(x, y, 
Plot = list(name = "", what = "lines", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = range(x, na.rm = TRUE), ylabels = 7, xlabels = "Auto", 
xtitle = "", ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), ...)
\S4method{timePlot}{POSIXt,numeric}(x, y, 
Plot = list(name = "", what = "lines", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = range(x, na.rm = TRUE), ylabels = 7, xlabels = "Auto", 
xtitle = "", ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), ...)
\S4method{timePlot}{numeric,numeric}(x, y, 
Plot = list(name = "", what = "lines", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = range(x, na.rm = TRUE), ylabels = 7, xlabels = "Auto", 
xtitle = "", ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), ...)
\S4method{timePlot}{integer,numeric}(x, y, 
Plot = list(name = "", what = "points", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = range(x, na.rm = TRUE) + c(-1, 1), ylabels = 7, xlabels = "Auto", 
xtitle = "Calendar Year", ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), xlabels.rotate = FALSE, ...)
\S4method{timePlot}{difftime,numeric}(x, y, 
Plot = list(name = "", what = "lines", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = range(x, na.rm = TRUE), ylabels = 7, xlabels = "Auto", 
xtitle = "Auto", ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), ...)
}
\arguments{
  \item{x}{
the time/date data.
}
  \item{y}{
the y-axis data.
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
  \item{xaxis.range}{
set the range of the x-axis. Set at January 1 through December 31 for \code{seasonPlot}.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set up x-axis labels. See \code{\link{datePretty}} for details for \code{timePretty}.
See \bold{Details} for details for \code{seasonPretty}.
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
rotate x-axis labels 90 degrees?
}
  \item{\dots}{
additional arguments for other methods.
}
}
\details{
The value for \code{xlabels} must be one of "full," the full month names; "abbrev," three-letter
abbreviations; or "letter," the first letter of the month. "Auto," the default, is "abbrev."
}
\value{
Information about the graph.
}
\note{
The function \code{timePlot} produces a time-series plot. The function \code{seasonPlot} produces
a plot of the annual cycle. There is no function in the USGSwsGraphs package that will automatically
transform time/date data to the correct seasonal value; use \code{dectime(x) - trunc(dectime(x))},
where \code{x} is the time/date variable.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{timePlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{xyPlot}}
}
\section{Methods}{
\describe{

\item{\code{signature(x = "Date", y = "numeric")}}{
Create a time-series plot for Date and numeric data.
}
\item{\code{signature(x = "POSIXt", y = "numeric")}}{
Create a time-series plot for POSIXt and numeric data.
}
\item{\code{signature(x = "numeric", y = "numeric")}}{
Create a time-series plot for dates in decimal format and numeric data.
}
\item{\code{signature(x = "integer", y = "numeric")}}{
Create a time-series plot for annual summaries of numeric data.
}
\item{\code{signature(x = "difftime", y = "numeric")}}{
Create a time-series plot for difftime and numeric data.
}
}}
\keyword{methods}
\keyword{hplot}
