\name{addBars}
\alias{addBars}
\title{
Bar Graph
}
\description{
Create a bar chart by adding bars to an existing graph.
}
\usage{
addBars(x, y, base = 0, Bars = list(name = "Auto", fill = "gray80", 
  outline = "black", width = "Auto", orientation = "stack"),
  current = list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE))
}
\arguments{
  \item{x}{
the x-coordinate data. Missing values are permitted but result in no bar.
}
  \item{y}{
the heights of the bars or y-coordinate data. Missing values are permitted but result in no bar.
}
  \item{base}{
extend the bars from this value.
}
  \item{Bars}{
parameters defining the characteristics of the bars. See \bold{Details}.
}
  \item{current}{
the current plot information. Typically, this would be the output from one of the graph creation functions like \code{xyPlot}.
}
}
\details{
The \code{Bars} argument must be a tagged list with these components:
\describe{
	\item{name}{a name describing the data; used in the explanation. If "Auto," then derive the
	name from the column names in \code{y}.}
	\item{fill}{the name of the color to fill each bar.}
	\item{outline}{the name of the color to draw the outline or border for each bar. If "none,"
	then no border is drawn.}
	\item{width}{the width of each bar proportional to the distance between bars. If 1, then the
	bars form a continuous filled area. The default is "Auto," which fills 2/3 of the distance.}
	\item{orientation}{the orientation of the bars.}
}
Only single bars are permitted in the current version. The \code{orientation} 
component of \code{Bars} is ignored.
}
\value{
The current plot information is returned invisibly.
}
\note{
Use of \code{addBars} adds 1 step to creating bar charts, but adds flexibility in
axis set up from existing high-level plotting functions such as \code{xyPlot} or
\code{timePlot}.
}
\seealso{
\code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{addXY}}, \code{\link{xyPlot}}
}
\keyword{aplot}

