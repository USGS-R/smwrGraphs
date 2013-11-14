\name{USGSwsGraphs-package}
\alias{USGSwsGraphs-package}
\alias{USGSwsGraphs}
\docType{package}
\title{
Graphing Functions
}
\description{
Functions to create high-quality graphs.\cr
These graphs meet or nearly meet the publications standards of the USGS.
They are intended to be a suite of functions that make producing complicated
graphs and figures relatively easy by passing much information about the plots
between functions so the user does not need to manage graphical information.
}
\details{
\tabular{ll}{
Package: \tab USGSwsGraphs\cr
Type: \tab Package\cr
Version: \tab 0.8\cr
Date: \tab 2013-09-10\cr
License: \tab file LICENSE\cr
Depends: \tab KernSmooth, USGSwsBase (>= 0.4), akima, methods\cr
Suggests: \tab USGSwsData (>= 0.6)\cr
}
Functions to set up and initialize the \code{USGSwsGraphs} environment:\cr
\code{\link{setGraph}}\cr
\code{\link{setLayout}}\cr
\code{\link{setPDF}}\cr
\code{\link{setPage}}\cr
\code{\link{setSplom}}\cr
\code{\link{setGD}}\cr
\cr
Main plotting functions:\cr
\code{\link{areaPlot}}\cr
\code{\link{biPlot}}\cr
\code{\link{boxPlot}}\cr
\code{\link{colorPlot}}\cr
\code{\link{contourPlot}}\cr
\code{\link{corGram}}\cr
\code{\link{dotPlot}}\cr
\code{\link{ecdfPlot}}\cr
\code{\link{histGram}}\cr
\code{\link{piperPlot}}\cr
\code{\link{probPlot}}\cr
\code{\link{qqPlot}}\cr
\code{\link{scalePlot}}\cr
\code{\link{splomPlot}}\cr
\code{\link{stiffPlot}}\cr
\code{\link{ternaryPlot}}\cr
\code{\link{timePlot}}\cr
\code{\link{transPlot}}\cr
\code{\link{xyPlot}}\cr
\code{\link{condition}}\cr
\cr
Functions to add features to a plot:\cr
\code{\link{addAnnotation}}\cr
\code{\link{addArea}}\cr
\code{\link{addAxisLabels}}\cr
\code{\link{addCaption}}\cr
\code{\link{addErrorBars}}\cr
\code{\link{addExplanation}}\cr
\code{\link{addGrid}}\cr
\code{\link{addLabel}}\cr
\code{\link{addPiper}}\cr
\code{\link{addStiff}}\cr
\code{\link{addTernary}}\cr
\code{\link{addSmooth}}\cr
\code{\link{addTable}}\cr
\code{\link{addTitle}}\cr
\code{\link{addXY}}\cr
\code{\link{labelPoints}}\cr
\code{\link{refLine}}\cr

}
\author{
Dave Lorenz <lorenz@usgs.gov>
}
\references{
Lorenz, D.L. in preparation, USGSwsGraphs---an R package for graphing hydrologic data, version 0.7.
}
\keyword{package}
\examples{
# For these examples, print to console
.pager <- options("pager")
options(pager="console")
# See the demo for examples of how to use the functions in this library.
demo(package="USGSwsGraphs")
# A simple listing of the vignettes in this package:
vignette(package="USGSwsGraphs")
options(.pager)
}
