\name{addTernary}
\alias{addTernary}
\title{
Add Detail to a Ternary Diagram
}
\description{
Adds points or lines to a Ternary Diagram.
}
\usage{
addTernary(x, y, z, Plot = list(name = "", what = "points", type = "solid", 
  width = "standard", symbol = "circle", filled = TRUE, size = 0.09, 
  color = "black"), 
 current = list())
}
\arguments{
  \item{x}{
the x-axis (bottom) data. Missing values are permitted, but result in breaks 
in the plotted data.
}
  \item{y}{
the y-axis (left side) data. Missing values are permitted, but result in breaks 
in the plotted data.
}
  \item{z}{
the z-axis (right side) data. Note that \code{x}, \code{y}, and \code{z} 
do not need to sum to the axis range. Missing values are permitted, but result 
in breaks in the plotted data.
}
  \item{Plot}{
parameters defining the characteristics of the plot. 
}
  \item{current}{
the current plot information. Typically, this would be the output from one of the graph creation functions like \code{ternaryPlot}.
}
}
\value{
Information about the graph.
}

\seealso{
\code{\link{ternaryPlot}}
}
\keyword{aplot}

