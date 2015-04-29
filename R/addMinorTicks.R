#' Add Axis Ticks
#' 
#' Add minor axis ticks to a graphs.
#' 
#' The \code{current} argument must contain a component named \code{yax} if
#' \code{which} is "left" or "right" or a component named \code{xax} if
#' \code{which} is "bottom" or "top." Those arguments are generally constructed
#' from functions like \code{linearPretty}.
#' 
#' The default placement of minor ticks is at the largest unit that lies
#' between the major ticks---if the difference between major ticks is an even
#' multiple of 2 or 5, then the minor interval will be that even multiple of 1,
#' otherwise it will be 1/10 that even multiple.
#' 
#' @param which which axis to label, must be one of "bottom," "left," "top," or
#' "right," "x," or "y." If \code{which} is "x," then add both bottom and top
#' minors ticks. If \code{which} is "y," then add both left and right minor
#' ticks.
#' @param current the current plot information, see \bold{Details}.
#' @param ticks the number of minor ticks to draw. If missing, then the default
#' number is used, see \bold{Details}.
#' @return The current plot information is returned invisibly.
#' @note In general, this should be used only with linear axes. Other axis
#' types can result in unexpected results.
#' @seealso \code{\link{linearPretty}}, \code{\link{addAxisLabels}},
#' \code{\link{addLabel}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- runif(25, .5, 9.5)
#' Y <- runif(25)
#' AA.pl <- xyPlot(X, Y)
#' addMinorTicks("bottom", AA.pl)
#' addMinorTicks("top", AA.pl)
#' # For more details of addMinorTicks see
#' vignette(topic="DateAxisFormats", package="smwrGraphs")
#' }
#' @export addMinorTicks
addMinorTicks <- function(which, current, ticks) {
	# Coding History:
	#    2014Jan31 DLLorenz Original coding.
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  which <- match.arg(which, c("bottom", "left", "top", "right", "x", "y"))
  plotsize <- par("pin")
  ticklen <- .04/min(plotsize)
  lwd <- frameWt()
	if(which %in% c("bottom", "top", "x")) {
		ax <- current$xax
	} else
		ax <- current$yax
  at <- ax$finegrid[!(ax$finegrid %in% ax$ticks)]
  if(!missing(ticks)) {
  	# Logic is not exactly straightforward, but easy coding
  	at <- apply(cbind(ax$ticks[-length(ax$ticks)], ax$ticks[-1L]),1L,
  							function(x) cumsum(rep(diff(x)/(ticks+1), ticks))+x[1])
  	at <- as.vector(at) # strip matrix attributes
  }
  if(which %in% c("bottom", "x")) {
  	axis(side=1L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  if(which %in% c("top", "x")) {
  	axis(side=3L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  if(which %in% c("left", "y")) {
  	axis(side=2L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  if(which %in% c("right", "y")) { 
  	axis(side=4L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  invisible(current)
}
