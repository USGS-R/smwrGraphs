\name{addStiff}
\alias{addStiff}
\alias{stiffPlot}
\title{
Stiff Diagrams
}
\description{
Add a Stiff diagram to an existing graph or create a tabular presentation of 
Stiff diagrams in a graph.
}
\usage{
addStiff(x, y, width, height, cations, anions, 
 Stiff = list(fill = "gray50", outline = "black", height = 2/3, bar = "black"), 
 xaxis.range = c(NA, NA), catlabels = "", anlabels = "", 
 current = list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE))
stiffPlot(cations, anions, Stiff = list(fill = "gray50", outline = "black",
  height = 2/3, bar = "black"), 
 yaxis.orient = "table", yaxis.order = "none", xaxis.range = c(NA, NA),
 ylabels = "Auto", xlabels = 7, catlabels = "Auto", anlabels = "Auto", 
 xtitle = "Milliequivalents per liter", ytitle = "", caption = "", 
 margin = c(NA, NA, NA, NA), ...)
}
\arguments{
  \item{x}{
the x-coordinates to place the center of each Stiff diagram. Missing values are permitted,
but result in no Stiff diagram.
}
  \item{y}{
the y-coordinates to place the center of each Stiff diagram. Missing values are permitted,
but result in no Stiff diagram.
}
  \item{width}{
the width in inches of the Stiff diagrams.
}
  \item{height}{
the height in inches of the Stiff diagrams.
}
  \item{cations}{
a matrix of cation data. Each row corresponds to the respective \code{x} and \code{y} value.
Missing values are not permitted for \code{addStiff}, but are permitted for
\code{stiffPlot} and result in no Stiff Diagram for that entry.
}
  \item{anions}{
a matrix of anion data. Each row corresponds to the respective \code{x} and \code{y} value.
Missing values are not permitted for \code{addStiff}, but are permitted for
\code{stiffPlot} and result in no Stiff Diagram for that entry.
}
  \item{Stiff}{
a list describing the Stiff diagram. See \bold{Details}.
}
  \item{xaxis.range}{
the range of the x-axis corresponding to \code{width} in the call to \code{addStiff} or
the range of the x-axis in the call to \code{stiffPlot}.
}
  \item{catlabels}{
labels for the vales of the cations. For \code{addStiff}, the labels are applied to each Stiff diagram and
for \code{stiffPlot}, the labels are stored and drawn on the explanation \code{addExplanation}.
}
  \item{anlabels}{
labels for the vales of the anions. For \code{addStiff}, the labels are applied to each Stiff diagram and
for \code{stiffPlot}, the labels are stored and drawn on the explanation \code{addExplanation}.
}
  \item{current}{
the current plotting parameters. Typically, this would be the output from one of the graph creation functions like \code{xyPlot}.
}
  \item{yaxis.orient}{
orientation of the y-axis values, must be either "table" or "grid." "Table" is sorted from top to bottom,
"grid" is sorted from bottom to top.
}
  \item{yaxis.order}{
the order of the y-axis values, must be one of "none," "ascending," or "descending."
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
}
\details{
The units of the cation and anion data are generally in milli-equivalents per liter.
}
\value{
Information about the graph.
}
\references{
Hem J.D., 1989, Study and interpretation of the chemical characteristics of natural water:
U.S. Geological Survey Water-Supply Paper 2254, 263 p.
}
\seealso{
\code{\link{stiffPlot}}, \code{\link{xyPlot}}
}
\keyword{hplot}
