#' Add Caption
#' 
#' Adds a caption at the bottom of the graph
#' 
#' @param caption the text of the caption for the graph
#' @return Nothing is returned.
#' @note Useful for adding 1-line captions.
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- runif(25)
#' Y <- runif(25)
#' AA.pl <- xyPlot(X, Y)
#' addCaption("Twenty five random points")
#' # See for examples of addCaption:
#' vignette(topic="GraphSetup", package="smwrGraphs")
#' demo(topic="AnnualFlowBarChart", package="smwrGraphs")
#' }
#' @export addCaption
addCaption <- function(caption='') {
	# Coding History:
	#    2009Apr23 DLLorenz Original coding and start of revisions
	#    2011May23 DLLorenz Conversion to R
	#    2012Sep18 DLLorenz Added long integers
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  lineoff <- par("mar")[1L]
  par(family='USGS')
  if(is.expression(caption) || caption != '')
    mtext(text=caption, side=1L, line=lineoff - 1, 
          at=par("usr")[1L], adj=0)
  invisible()
}
