\name{piperPlot}
\alias{piperPlot}

\title{
Piper Diagram
}
\description{
Produce a Piper Diagram
}
\usage{
piperPlot(xCat, yCat, zCat, xAn, yAn, zAn, Plot = list(name = "", what = "points", 
type = "solid", width = "standard", symbol = "circle", filled = TRUE, size = 0.09, 
color = "black"), axis.range = c(0, 100), num.labels = 6, ticks = FALSE, 
grids = !ticks, xCat.title = "Calcium", yCat.title = "Magnesium", 
zCat.title = "Sodium plus Potassium", xAn.title = "Chloride, Fluoride, Nitrite plus Nitrate", 
yAn.title = "Carbonate plus Bicarbonate", zAn.title = "Sulfate", 
x.yCat.title = "Calcium plus Magnesium", x.zAn.title = "Sulfate plus Chloride", 
units.title = "Percent", caption = "", margin = c(NA, NA, NA, NA))
}

\arguments{
  \item{xCat}{
data for the cation x-axis, generally calcium.
}
  \item{yCat}{
data for the cation y-axis, generally magnesium.
}
  \item{zCat}{
data for the cation z-axis, generally sodium plus potasium.
}
  \item{xAn}{
data for the anion x-axis, generally chloride plus other minor constituents.
}
  \item{yAn}{
data for the anion y-axis, generally carbonate plus bicarbonate.
}
  \item{zAn}{
data for the anion z-axis, generally sulfate.
}
  \item{Plot}{
control parameters of the plot, see \code{link{setMultiPlot}} and \bold{Details} for details.
}
  \item{axis.range}{
the range of the axes. Must be either c(0, 1) or c(0, 100).
}
  \item{num.labels}{
the number of labels to draw on each axis. Best
  selections are 2 giving (0, 100), 3 (0, 50, 100), 5 (0, 25, 50, 75, 100), or
  6 (o, 20, 40, 60, 80, 100).
}
  \item{ticks}{
draw ticks?
}
  \item{grids}{
draw grid lines?
}
  \item{xCat.title}{
title for the cation x-axis.
}
  \item{yCat.title}{
title for the cation y-axis.
}
  \item{zCat.title}{
title for the cation x-axis.
}
  \item{xAn.title}{
title for the anion x-axis.
}
  \item{yAn.title}{
title for the anion y-axis.
}
  \item{zAn.title}{
title for the anion z-axis.
}
  \item{x.yCat.title}{
title for the cation x- and y-axis for the central Piper graph.
}
  \item{x.zAn.title}{
title for the anion x- and z-axis for the central Piper graph.
}
  \item{units.title}{
the units titles, should be either "Percent"
  of "Proportion" depending on \code{axis.range}.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins---
  ignored, included for consistency with other plotting functions in this
  package.
}
}
\details{
The \code{what} component of the \code{Plot} argument must be either "points" or "none."
}
\value{
Information about the graph and current plot.
}
\references{
Hem J.D., 1989, Study and interpretation of the chemical characteristics of natural water:
U.S. Geological Survey Water-Supply Paper 2254, 263 p.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{piperPlot}.
}
\seealso{
\code{\link{setPage}}, \code{link{setMultiPlot}}, \code{\link{ternaryPlot}}, \code{\link{addPiper}}
}
\keyword{ hplot }
