#' Axis Labels
#' 
#' Adds text in the margin for axis labels.
#' 
#' 
#' @param label The text or expression to add to the graph.
#' @param x the axis location in the correct user units.
#' @param side the side to place \code{label}. Must be "bottom," "left," "top,"
#' or "right." Only the first letter is necessary.
#' @param size the sized of the text in points.
#' @param distance the distance from the axis, in lines of text.
#' @param justification defines the placement of the text relative to \code{x}
#' if \code{orientation} is "parallel" and relative to \code{distance} if if
#' \code{orientation} is "perdendicular." Must be one of "left," "center," or
#' "right."
#' @param orientation the orientation of the label relative to the axis. Must
#' be either "parallel" or "perdendicular."
#' @param current the current plot parameters. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return Nothing is returned.
#' @seealso \code{\link{mtext}}, \code{\link{plotmath}} for example
#' expressions, \code{\link{xyPlot}}
#' @keywords aplot
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
