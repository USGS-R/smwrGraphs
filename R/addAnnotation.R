#' Add Text to a Graph
#' 
#' Adds text to a Graph.
#' 
#' This function places only a single text on the graph for each call.\cr A
#' leader from \code{x}, \code{y} to \code{leaderx}, \code{leadery} if
#' \code{leaderx} is not \code{NULL}.
#' 
#' @param x the x-axis placement of \code{annotation}.
#' @param y the y-axis placement of \code{annotation}.
#' @param annotation the text.
#' @param leaderx draw leader from \code{x} to \code{leaderx}.
#' @param leadery draw leader from \code{y} to \code{leadery}.
#' @param leadercol the color of the leader.
#' @param angle the angle to rotate the text.
#' @param justification the justification of the text relative to \code{x},
#' \code{y}. Must be one of "left," "center," or "right."
#' @param size size of the text in points, the default is the current point size.
#' @param position the vertical location of the text. Must be one of "above,"
#' "below," or "center."
#' @param current the current plot controls. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return The current plot information is returned invisibly.
#' @seealso \code{\link{labelPoints}}, \code{\link{addTable}},
#' \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' # See for examples of addAnnotation:
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' demo(topic="TopAxisExample", package="smwrGraphs")
#' }
#' @export addAnnotation
addAnnotation <- function(x, y, annotation, # data
                          leaderx=NULL, leadery=NULL, leadercol="black", # leader controls
                          angle=0, justification="left", size=60*par("csi"),
                          position="above", # placement control
                          current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                            xaxis.log=FALSE)) { # current plot parameters 
	# Coding History:
	#    2008Aug01 DLLorenz Original coding and begin of tweaks.
	#    2011Jan20 DLLorenz Conversion to R
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Sep18 DLLorenz Added long integers
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  ## convert to usr units
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                 current$ytrans, current$ytarg)
  x <- transData(x, current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  on.exit(par(cex=1)) # restore default text size
  csi <- par("csi") / 1.2 # converts to actual size of character inches
  par(cex = size / 72 / csi)
  position <- match.arg(position, c("above", "below", "center"))
  justification <- (pmatch(justification, c("left", "center", "right"),
                           nomatch=1L) - 1)/2
  ## Compute the inches per user unit
  pin <- par('pin')
  usr <- par('usr')
  uin <- pin/c(usr[2L] - usr[1L], usr[4L] - usr[3L])
  ## A small adjustment is needed to y to adjust for assymetric font/line
  ##  relation.
  ytxt <- y + 0.01/uin[2L]
  xtxt <- x + 0.08/uin[1L] * (0.5 - justification)
  if(position == "above") # Adjust y up
    ytxt <- ytxt + 0.04/uin[2L] + strheight(annotation)/2
  else if(position == "below") # Adjust y down
    ytxt <- ytxt - 0.04/uin[2L] - strheight(annotation)/2
  ## Place the annotation
  text(xtxt, ytxt, annotation, srt=angle, adj=justification, family='USGS')
  if(!is.null(leaderx)) {
    leadery <- transData(leadery, current$yaxis.log, current$yaxis.rev,
                         current$ytrans, current$ytarg)
    leaderx <- transData(leaderx, current$xaxis.log, FALSE,
                         current$xtrans, current$xtarg)
    segments(x, y, leaderx, leadery, col=leadercol, lwd=stdWt())
  }
  invisible(current)
}
