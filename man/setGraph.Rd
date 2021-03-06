% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setGraph.R
\name{setGraph}
\alias{setGraph}
\title{Set Graph}
\usage{
setGraph(graphNum, layout, noTicks = NULL)
}
\arguments{
\item{graphNum}{the number of the graph.}

\item{layout}{the layout of the graph.}

\item{noTicks}{suppress ticks on a specific axis.}
}
\value{
The parameters of the margin of the plot area.
}
\description{
Sets up a specific graph on a graphics device.
}
\note{
This function is called using the information generated by
\code{setLayout}. It invisibly sets up the graphics device for the next
graph.
}
\examples{
\dontrun{
# See for examples of setGraph:
vignette(topic="BoxPlots", package="smwrGraphs")
vignette(topic="GraphGallery", package="smwrGraphs")
vignette(topic="GraphSetup", package="smwrGraphs")
vignette(topic="PiperPlot", package="smwrGraphs")
demo(topic="HydroPrecip", package="smwrGraphs")
demo(topic="PiperScript", package="smwrGraphs")
}
}
\seealso{
\code{\link{setLayout}}
}
\keyword{dplot}
