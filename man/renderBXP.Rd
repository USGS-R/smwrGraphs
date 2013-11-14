\name{renderBXP}
\alias{renderBXP}
\alias{renderBoxPlot}
\title{
Boxplot
}
\description{
Draw a boxplot (support functions).
}
\usage{
renderBXP(x, width, z, draw.RL = TRUE, fill = "none")
renderBoxPlot(xtoplot, stats, Box, explan, expz, yaxis.log=FALSE, yrange=c(NA,NA), 
xrange = range(xtoplot) + c(-1, 1), ylabels="Auto", xlabels="Auto", xlabels.rotate=FALSE, 
xtitle="", ytitle="",  caption="", margin=c(NA,NA,NA,NA))
}
\arguments{
  \item{x}{
the x-coordinate of the box.
}
  \item{width}{
the width of the box in x-axis units.
}
  \item{z}{
a list containing the statistics for the individual boxplot.
}
  \item{fill}{
a character string describing the fill color for the box or "none" for no fill.
}
  \item{draw.RL}{
draw the reporting level?
}
  \item{xtoplot}{
the x-axis locations for each boxplot.
}
  \item{stats}{
a list containing the statistics for the boxplots.
}
  \item{Box}{
a list containing the control info for the boxplots.
}
  \item{explan}{
a list containing the information for an explanation.
}
  \item{expz}{
a list containing the information for an explanation of the boxplot.
}
  \item{yaxis.log}{
logical: use a log transfor for the data nad the y-axis?
}
  \item{yrange}{
set the y-axis range.
}
  \item{xrange}{
set the x-axis range.
}  \item{ylabels}{
either "Auto," the approximate number of labels, or the actual labels to use for the y-axis.
}
  \item{xlabels}{
either "Auto" or the x-axis labels for each boxplot.
}
  \item{xlabels.rotate}{
logical: rotate the x-axis lables by 90 degrees?
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
\note{
The function \code{renderBXP} draws a single boxplot in a graph. The function \code{renderBoxPlot} is 
called from each method function for \code{boxPlot} to produce the boxplot.
}
\seealso{
\code{\link{boxPlot}}
}
\keyword{aplot}
