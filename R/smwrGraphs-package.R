#' Graphing Functions
#' 
#' Functions to create high-quality graphs.\cr These graphs meet or nearly meet
#' the publication standards for illustrations of the U.S.  Geological Survey
#' (USGS) (U.S. Geological Survey, written commun., 2012). They are intended to
#' be a suite of integrated functions that make producing graphs and figures
#' relatively easy by passing much information about the plots between
#' functions so the user does not need to manage graphical information.
#' 
#' \tabular{ll}{ 
#' Package: \tab smwrGraphs\cr 
#' Type: \tab Package\cr 
#' Version: \tab 1.0.10\cr 
#' Date: \tab 2015-05-10\cr 
#' License: \tab CC0\cr 
#' Depends: \tab smwrBase (>= 1.0.0), methods\cr 
#' Imports: \tab KernSmooth, akima, lubridate\cr
#' Suggests: \tab smwrData (>= 1.0.0), dataRetrieval\cr }
#' The functions in the \code{smwrGraphs} package
#' are an integrated suite of functions that facilitate the production of
#' graphs that nearly meet USGS publication standards for illustrations (U.S.
#' Geological Survey, written commun., 2012). Those standards include line
#' weight, tick placement, labels, font size, and layout of the explanation.
#' The font used in production very closely matches the standard Univers
#' Condensed, and was selected because of its broad availability on many
#' computer platforms.\cr
#' 
#' Use of base \code{R} or other gaphics functions can result in inconsistent
#' lineweights, font sizes and styles, and can require manual manipulation of
#' the explanation. The Programmer's Guide section in Lorenz (2015) shows 
#' examples of calls to lower level graphics functions in base \code{R} 
#' that produce consistent graphics products.\cr
#' 
#' Functions to set up and initialize the \code{smwrGraphs} environment:\cr
#' \code{\link{setGD}}\cr
#' \code{\link{setGraph}}\cr 
#' \code{\link{setKnitr}}\cr
#' \code{\link{setLayout}}\cr 
#' \code{\link{setPDF}}\cr
#' \code{\link{setPage}}\cr 
#' \code{\link{setPNG}}\cr
#' \code{\link{setRStudio}}\cr
#' \code{\link{setRtMargin}}\cr
#' \code{\link{setSplom}}\cr 
#' \code{\link{setSweave}}\cr
#' \code{\link{setTopMargin}}\cr\cr
#' Main plotting functions:\cr 
#' \code{\link{areaPlot}}\cr
#' \code{\link{biPlot}}\cr 
#' \code{\link{boxPlot}}\cr 
#' \code{\link{colorPlot}}\cr
#' \code{\link{contourPlot}}\cr 
#' \code{\link{corGram}}\cr
#' \code{\link{dotPlot}}\cr 
#' \code{\link{ecdfPlot}}\cr 
#' \code{\link{histGram}}\cr
#' \code{\link{piperPlot}}\cr 
#' \code{\link{probPlot}}\cr 
#' \code{\link{qqPlot}}\cr
#' \code{\link{reportGraph}}\cr
#' \code{\link{scalePlot}}\cr 
#' \code{\link{splomPlot}}\cr
#' \code{\link{stiffPlot}}\cr 
#' \code{\link{preSurface}} 
#' \code{\link{surfacePlot}} \cr
#' \code{\link{ternaryPlot}}\cr
#' \code{\link{timePlot}}\cr 
#' \code{\link{transPlot}}\cr 
#' \code{\link{xyPlot}}\cr
#' \code{\link{condition}}\cr\cr 
#' Functions to add features to a plot:\cr
#' \code{\link{addAnnotation}}\cr 
#' \code{\link{addArea}}\cr
#' \code{\link{addAxisLabels}}\cr 
#' \code{\link{addCaption}}\cr
#' \code{\link{addErrorBars}}\cr 
#' \code{\link{addExplanation}}\cr
#' \code{\link{addGrid}}\cr 
#' \code{\link{addLabel}}\cr 
#' \code{\link{addPiper}}\cr
#' \code{\link{addStiff}}\cr 
#' \code{\link{addTernary}}\cr
#' \code{\link{addSmooth}}\cr 
#' \code{\link{addTable}}\cr
#' \code{\link{addTitle}}\cr 
#' \code{\link{addXY}}\cr
#' \code{\link{labelPoints}}\cr 
#' \code{\link{refLine}}\cr
#' Selected Miscellaneous Functions:\cr
#' \code{\link{copyDemo}}\cr
#' \code{\link{strip.blanks}}\cr
#' @name smwrGraphs-package
#' @aliases smwrGraphs-package smwrGraphs
#' @docType package
#' @author Dave Lorenz <lorenz@@usgs.gov>
#' @references Lorenz, D.L. in preparation, smwrGraphs---an R package for
#'graphing hydrologic data, version 1.0.10.\cr
#'U.S. Geological Survey, 2012, Author\verb{'}s guide to standards for U.S. 
#'Geological Survey page-size illustrations, 37 p.
#' @keywords package
#' @import smwrBase
#' @import methods
#' @examples
#' 
#' # For these examples, print to console
#' .pager <- options("pager")
#' options(pager="console")
#' # See the demo for examples of how to use the functions in this library.
#' demo(package="smwrGraphs")
#' # A simple listing of the vignettes in this package:
#' vignette(package="smwrGraphs")
#' options(.pager)
#' 
NULL
.onAttach <- function(libname, pkgname) {
	packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")}
