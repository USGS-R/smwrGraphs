#' Reference Line
#' 
#' Adds a reference line (vertical, horizontal, or regression) to a graph.
#' 
#' 
#' @param horizontal draw horizontal lines at the specified values.
#' @param vertical draw vertical lines at the specified values.
#' @param coefficients draw a fitted line from the coefficients of a regression
#' model.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters. The argument \code{what} is
#' forced to "lines."
#' @param current the current parameters of the graph. Typically, this would be
#' the output from one of the graph creation functions like \code{xyPlot}.
#' @param xrange limit x-axis range for horizontal or regression lines
#' @param yrange limit y-axis range for vertical lines
#' @param log10 logical, if \code{TRUE}, then log base 10 transform used in the 
#' regression model, otherwise either the natural log was used or no transform.
#' @return Information about the graph.
#' @seealso \code{\link{addXY}}, \code{\link{addSmooth}}, \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' setGD()
#' xyPlot(X, Y)
#' # Add the 1:1 line
#' refLine(coefficient=c(0,1))
#' # For more details of refLine see
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' demo(topic="Coplot-complexScatterPlot", package="smwrGraphs")
#' }
#' @export refLine
refLine <- function(horizontal, vertical, coefficients, # data, at least one must be used
                  Plot=list(name="", what='lines', type='solid',
                    width='standard', symbol='circle', filled=TRUE,
                    size=0.09, color='black'), # plot controls
                  current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                    xaxis.log=FALSE), # current plot parameters
                  xrange=c(NA,NA), yrange=c(NA,NA), # limit range of lines
                  log10=FALSE) { # was log base 10 used in any transforms
	# Coding History:
	#    2008Jul02 DLLorenz Original coding.
	#    2010Nov29 DLLorenz Conversion to R
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Aug02 DLLorenz Start of bug fixes
	#    2011Oct24 DLLorenz Tweaks for package
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  Plot <- setPlot(Plot, name="", what='lines', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black') # force defaults if not set
  Plot$what <- 'lines' # Force this
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
  plotPars <- explan$current
  ## set x- and y-range
  xrange <- transData(xrange, current$xaxis.log, FALSE,
                      current$xtrans, current$xtarg)	
  yrange <- transData(yrange, current$yaxis.log, current$yaxis.rev,
                      current$ytrans, current$ytarg)
  xrange[is.na(xrange)] <- par('usr')[1:2][is.na(xrange)]
  yrange[is.na(yrange)] <- par('usr')[3:4][is.na(yrange)]
  ## Do the correct requests
  if(!missing(horizontal)) {
    ## transform it
    y <- transData(horizontal, current$yaxis.log, current$yaxis.rev,
                  current$ytrans, current$ytarg)
    ## draw it
    segments(xrange[1], y, xrange[2], y, lwd=plotPars$lwd, lty=plotPars$lty,
             col=plotPars$col)
  }
  if(!missing(vertical)) {
    ## transform it
    x <- transData(vertical, current$xaxis.log, FALSE,
                  current$xtrans, current$xtarg)
    ## draw it
    segments(x, yrange[1], x, yrange[2], lwd=plotPars$lwd, lty=plotPars$lty,
             col=plotPars$col)
  }
  if(!missing(coefficients)) {
    ## adjust for log10 transforms
    if(current$yaxis.log && !log10) # the coefficients are in natural logs
      coefficients <- coefficients * log10(exp(1))
    if(current$xaxis.log && !log10) # the coefficients are in natural logs
      coefficients[2] <- coefficients[2] / log10(exp(1))
    ## compute the y data and draw the line
    y <- coefficients[1] + coefficients[2] * xrange
    segments(xrange[1], y[1], xrange[2], y[2], lwd=plotPars$lwd,
             lty=plotPars$lty, col=plotPars$col)
  }
  invisible()
  ## x and y are not replaced by this function
  current$explanation <- explan
  invisible(current)
}
