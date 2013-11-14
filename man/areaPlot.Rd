\name{areaPlot}
\alias{areaPlot}

\title{
Shaded Area Plot
}
\description{
Produces an plot where the area between lines is filled with color.
}
\usage{
areaPlot(x, y, Areas = list(name = "Auto", fillDir = "between", base = "Auto", lineColor = "black", 
fillColors = "pastel"), yaxis.log = FALSE, yaxis.range = c(NA, NA), xaxis.log = FALSE, 
xaxis.range = c(NA, NA), ylabels = 7, xlabels = "Auto", xtitle = "", ytitle = "", caption = "", 
margin = c(NA, NA, NA, NA))
}

\arguments{
  \item{x}{
x-axis coordinates in increasing order.

}
  \item{y}{
a numeric matrix of y-axis coordinates.
}
  \item{Areas}{
a list with these components: name, "Auto" (derive names from y colnames or
  names of areas; fillDir, "between" or "under"; lineColor, color
  of lines; fillColor, either a name of a color sequence generator or
  spcific color names for each area.
}
  \item{yaxis.log}{
logical: log transform y axis?
}
  \item{yaxis.range}{
set y-axis range.
}
  \item{xaxis.log}{
logical: log transform x axis?
}
  \item{xaxis.range}{
set x-axis range.
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
the caption for the graph.
}
  \item{margin}{
set up the plot area margins.
}
}
\value{
Information about the graph
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{areaPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{addArea}}, \code{\link{colorPlot}}, \code{\link{xyPlot}}, 
}

\keyword{ hplot }
