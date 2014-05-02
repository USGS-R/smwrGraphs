\name{scalePlot}
\alias{scalePlot}

\title{
Scale Plot
}
\description{
Produce a graph controling aspect ratio of the x- and y-axes.
}
\usage{
scalePlot(x, y, scale = 1, Plot = list(name = "", what = "lines", type = "solid", 
width = "standard", symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), xaxis.log = FALSE, 
xaxis.range = c(NA, NA), ylabels = 7, xlabels = 7, xtitle = deparse(substitute(x)), 
ytitle = deparse(substitute(y)), caption = "", margin = c(NA, NA, NA, NA))
}

\arguments{
  \item{x}{
the x-axis data.
}
  \item{y}{
the y-axis data.
}
  \item{scale}{
the y/x ratio. See \bold{Details}.
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
set the range of the y axis.
}
  \item{xaxis.log}{
log-transform the x axis?
}
  \item{xaxis.range}{
set the range of the x axis.
}
  \item{ylabels}{
set the y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set the y-axis labels. See \code{\link{linearPretty}} for details.
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
the parameters of the margin of the plot area.
}
}
\value{
Information about the graph.
}
\details{
The \code{scale} argument sets the scaling ratio of the y-axis to the x-axis. For
latitude and longitude data, set the scale to 1/cos(midlat/180*pi), where midlat is
the midrange of the latitude.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{scalePlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{xyPlot}}
}
\keyword{ hplot }
