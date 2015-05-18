#' Label Points
#' 
#' Label points on a graph.
#' 
#' 
#' @param x the x-axis data. Missing values are permitted, but ignored.
#' @param y the y-axis data. Missing values are permitted, but ignored.
#' @param labels the text labels, must be the same length as x and y. Missing
#' values are permitted, but ignored.
#' @param dir the direction relative to the point to place the label.
#' @param offset the relative offset from the point.
#' @param size character size in points.
#' @param color the color of the labels.
#' @param current the current plot controls. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return A list containing \code{x}, \code{y}, and \code{labels}.
#' @seealso \code{\link{addAnnotation}},, \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' xyPlot(X, Y)
#' # Label the first point
#' labelPoints(X[1], Y[1], "First")
#' # For more details of labelPoints see
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' }
#' @export labelPoints
labelPoints <- function(x, y, labels, # data
                        dir='E', offset=0.75, size=8, # placement control
                        color='black',
                        current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                          xaxis.log=FALSE)) { # current plot parameters 
	# Coding History:
	#    2008Jun28 DLLorenz Original coding and begin tweaks.
	#    2010Mar15 DLLorenz Added "C" dir, which centers the text
	#    2010Mar20 DLLorenz Added Color option
	#    2010Nov29 DLLorenz Conversion to R
	#    2011Sep01 DLLorenz Fixed typo
	#    2011Oct24 DLLorenz Tweaks for package
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  ## convert to usr units
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- transData(x, current$xaxis.log, FALSE,
                   current$xtrans, current$xtarg)
  ## get the expansion factor for font size
  Cex <- (size / 72) / par('csi')[2]
  ## get the size of the font in usr units
  cxy <- par('cxy')
  ## Because of the placement of letters relative to the character size,
  ## the y-values must be offset by a little
  y <- y + 0.15 * cxy[2]
  ## force dir to be the same length as y and code each direction
  dir <- rep(casefold(dir, TRUE), length.out=length(y))
  hjust <- c(.5, 0, 0, 0, .5, 1, 1, 1, .5)
  names(hjust) <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "C")
  just.horiz <- hjust[dir]
  ## compute the horizontal and vertical offsets
  hoff <- c(0, 1, 1, 1, 0, -1, -1, -1, 0)
  names(hoff) <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "C")
  x <- x + hoff[dir] * offset * cxy[1]
  voff <- c(1, 1, 0, -1, -1, -1, 0, 1, 0)
  names(voff) <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "C")
  y <- y +  voff[dir] * offset * cxy[2]
  ## force labels to be character
  labels <- as.character(labels)
  ## must do each point individually to account for adj
  for(i in seq(along=y))
    text(x[i], y[i], labels[i], adj=just.horiz[i], col=color, cex=Cex,
         family='USGS')
  retval <- list(x=x, y=y, labels=labels)
  invisible(retval)
}
