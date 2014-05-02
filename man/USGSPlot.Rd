\name{USGSPlot}
\alias{xyPlot}
\alias{timePlot}
\alias{seasonPlot}
\alias{dotPlot}
\alias{colorPlot}
\title{
Plot Data
}
\description{
Create a line or scatter plot.
}
\usage{
colorPlot(x, y, color, Plot=list(),
yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
xaxis.log=FALSE, xaxis.range=c(NA,NA),
ylabels=7, xlabels=7, xtitle="", ytitle="",
caption="", margin=c(NA, NA, NA, NA), ...)

dotPlot(x, y, Plot=list(),
yaxis.orient="", yaxis.order="", yaxis.grid=TRUE,
xaxis.log=FALSE, xaxis.range=c(NA,NA),
ylabels="", xlabels=7, xtitle="", ytitle="",
caption="", margin=c(NA, NA, NA, NA), ...)

seasonPlot(x, y, Plot=list(),
yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
xaxis.range="", ylabels=7, xlabels=7, xtitle="", ytitle="",
caption="", margin=c(NA, NA, NA, NA), ...)

timePlot(x, y, Plot=list(),
yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
xaxis.range=range(x, na.rm=TRUE),
ylabels=7, xlabels=7, xtitle="", ytitle="",
caption="", margin=c(NA, NA, NA, NA), ...)

xyPlot(x, y, Plot=list(),
yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
xaxis.log=FALSE, xaxis.range=c(NA,NA),
ylabels=7, xlabels=7, xtitle="", ytitle="",
caption="", margin=c(NA, NA, NA, NA),...)
}
\arguments{
  \item{x}{
the x-coordinate data.
}
  \item{y}{
the y-coordinate data.
}
  \item{color}{
either the color for each point, or a discrete variable that can be mapped to a color.
}
  \item{Plot}{
a list describing what to plot.
}
  \item{yaxis.log}{
log-transform the y-axis?
}
  \item{yaxis.rev}{
reverse the y-axis?
}
  \item{yaxis.range}{
set the range for the y-axis.
}
  \item{xaxis.log}{
log-transform the x-axis?
}
  \item{xaxis.range}{
set the range for the y-axis or the type of year (calendar, water, or climate) for \code{seasonPlot}.
}
  \item{ylabels}{
the approximate number of labels for the y-axis.
}
  \item{xlabels}{
the approximate number of labels for the x-axis.
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
the margin for the plot area.
}
  \item{yaxis.orient}{
a character string indicating the orientation of the y-axis, either "table" or "grid."
}
  \item{yaxis.order}{
a character string indicating the order of the y-axis.
}
  \item{yaxis.grid}{
a character string indicating the order of the y-axis.
}
  \item{\dots}{
other arguments for specific methods.
}
}

\value{
Information about the plot.
}

\seealso{
\code{\link{colorPlot-methods}}, \code{\link{dotPlot-methods}}, \code{\link{seasonPlot-methods}}, \code{\link{timePlot-methods}}, \code{\link{xyPlot-methods}}
}
\keyword{hplot}
