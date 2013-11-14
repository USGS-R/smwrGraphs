\name{addSmooth}
\alias{addSmooth}
\alias{addSmooth.default}
\alias{addSmooth.list}
\title{
Compute and Add a Smooth Line
}
\description{
Computes a smooth line from x and y data and adds the line to a graph.
}
\usage{
addSmooth(x, y, Smooth = "loess.smooth", ..., Plot = list(name = "", what = "lines", 
type = "solid", width = "standard", color = "black"), current = list(yaxis.log = FALSE, 
yaxis.rev = FALSE, xaxis.log = FALSE))
\method{addSmooth}{default}(x, y, Smooth = "loess.smooth", ..., Plot = list(name = "", what = "lines", 
type = "solid", width = "standard", color = "black"), current = list(yaxis.log = FALSE, 
yaxis.rev = FALSE, xaxis.log = FALSE))
\method{addSmooth}{list}(x, y, Smooth = "loess.smooth", ..., Plot = list(name = "", what = "lines", 
type = "solid", width = "standard", color = "black"), current = list(yaxis.log = FALSE, 
yaxis.rev = FALSE, xaxis.log = FALSE))
}

\arguments{
  \item{x}{
the x-axis data. For method \code{list}, x is a list that contains components
\code{x} and \code{y} and the \code{y} argument is not used. Missing values are 
permitted but ignored.
}
  \item{y}{
the y-axis data. Missing values are permitted but ignored.
}
  \item{Smooth}{
the name of the smoothing function.
}
  \item{\dots}{
additional parameters for the function names in \code{Smooth}.
}
  \item{Plot}{
parameters defining the characteristics of the plot.
}
  \item{current}{
the current plot information. Typically, this would be the output from one of the graph creation functions like \code{xyPlot}.
}
}

\value{
The current plot information.
}
\seealso{
\code{\link{addXY}}, \code{\link{xyPlot}}
}
\keyword{aplot}
