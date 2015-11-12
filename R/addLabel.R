#' Axis Labels
#' 
#' Adds text in the margin for axis labels.
#' 
#' 
#' @param label the text or expression to add to the graph.
#' @param x the axis location in the correct user units.
#' @param side the side to place \code{label}. Must be "bottom," "left," "top,"
#' or "right." Only the first letter is necessary.
#' @param size the size of the text in points.
#' @param distance the distance from the axis, in lines of text.
#' @param justification defines the placement of the text relative to \code{x}
#' if \code{orientation} is "parallel" and relative to \code{distance} if
#' \code{orientation} is "perpendicular." Must be one of "left," "center," or
#' "right."
#' @param orientation the orientation of the label relative to the axis. Must
#' be either "parallel" or "perpendicular."
#' @param current the current plot parameters. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return Nothing is returned.
#' @note In general, all functions that create plots will draw the necessary
#' axes. This function should be used only to add axis labels to an unlabeled
#' axis. Axis labels can be suppressed by setting up the margins with negative
#' values or, for some functions, special arguments to \code{xlabels} or 
#' \code{ylabels}.
#' @seealso \code{\link{addAxisLabels}},
#Flip for production/manual
#'\code{\link[graphics]{mtext}} and \code{\link[grDevices]{plotmath}} 
#\code{mtext}} (in graphics package), \code{plotmath} (in grDevices package)
#' for example expressions, 
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- as.POSIXct(c("2010-12-22 10:30", "2010-12-28 13:45", 
#' "2011-01-05 9:30", "2011-01-07 14:50"))
#' Y <- runif(4)
#' setGD()
#' AA.pl <- timePlot(X, Y, Plot=list(what="points"))
#' # Insert verical bar between years
#' addLabel("|", as.Date("2011-01-01"), distance=1.2)
#' # For more details of addLabel see
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' demo(topic="AnnualFlowBarChart", package="smwrGraphs")
#' }
#' @export addLabel
addLabel <- function(label, x, side="bottom", size="Auto", distance=0.2,
                   justification="center", orientation="parallel",
                   current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                     xaxis.log=FALSE)) { # current plot parameters 
	# Coding History:
	#    2012Feb15 DLLorenz Original coding
	#    2012Sep18 DLLorenz Added long integers
	#    2012Nov14 DLLorenz Bug Fix
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  ## convert to usr units
  side <- pmatch(side, c("bottom", "left", "top", "right"))
  if(side %in% c(1L, 3L))
    x <- transData(x, current$xaxis.log, FALSE,
                   current$xtrans, current$xtarg)
  else
    x <- transData(x, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  ## If a size is specified, then change the text size
  if(is.numeric(size)) {
    on.exit(par(cex=1)) # restore default text size
    csi <- par("csi") / 1.2 # converts to actual size of character inches
    par(cex = size/72 / csi)
  }
  orientation <- (pmatch(orientation, c("parallel", "perpendicular")) - 1L) * 2L
  justification <- (pmatch(justification, c("left", "center", "right"),
                           nomatch=1L) - 1)/2
  ## adjust away from the bottom/right if the height is greater than 1 line
  if(orientation == 0L && side %in% c(1L, 4L)) {
    lineSize <- par("cin")[2L]
    textSize <- strheight("M", units="in")
    labelSize <- strheight(label, units="in")
    distance <- distance + (labelSize - textSize) / lineSize
  }
  # These assume level 1
  mtext(label, side=side, line=distance, at=x, adj=justification,
        family="USGS", las=orientation, cex=7/8)
  invisible()
}
