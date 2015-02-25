#' Add Detail to a Ternary Diagram
#' 
#' Adds points or lines to a Ternary Diagram.
#' 
#' 
#' @param x the x-axis (bottom) data. Missing values are permitted, but result
#' in breaks in the plotted data.
#' @param y the y-axis (left side) data. Missing values are permitted, but
#' result in breaks in the plotted data.
#' @param z the z-axis (right side) data. Note that \code{x}, \code{y}, and
#' \code{z} do not need to sum to the axis range. Missing values are permitted,
#' but result in breaks in the plotted data.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{ternaryPlot}.
#' @return Information about the graph.
#' @seealso \code{\link{ternaryPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' # See for examples of addTernary:
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' }
#' @export addTernary
addTernary <- function(x, y, z,# data (need not sum to 100)
                       Plot=list(name="", what='points', type='solid',
                         width='standard', symbol='circle', filled=TRUE,
                         size=0.09, color='black'), # plot controls
                       current=list()) { # current parameters
	# Coding history:
	#    2012Nov02 DLLorenz Original coding and revisions.
	#    2014Jun25 DLLorenz Converted to roxygen
  ## 
  ## Process plot controls
  Plot <- setPlot(Plot, name="", what="points", type="solid",
                  width="standard", symbol="circle", filled=TRUE,
                  size=0.09, color="black")
  explan <- setExplan(Plot, current$explanation)
  ## Normalize the data according to range
  current <- setDefaults(current, axis.range=c(0, 100), orient="c")
  axis.range <- current$axis.range
  tsum <- sumComposition(x, y, z, Range=axis.range[2L])
  Data <- ternarySubplot(tsum[, 1L], tsum[, 2L], tsum[, 3L],
                         axis.range=axis.range, orient=current$orient,
                         plot=FALSE)
  points(Data$x, Data$y, type=explan$current$type,
         lwd=explan$current$lwd, lty=explan$current$lty,
         pch=explan$current$pch, cex=explan$current$cex,
         col=explan$current$col, bg=explan$current$col)
  ## Note that plot info does not pertain here
  retval <- list(x=Data$x, y=Data$y, Data=tsum, orient=current$orient,
                 axis.range=axis.range, explanation=explan)
  invisible(retval)
}
