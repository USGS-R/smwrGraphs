# add a reference line
#
# Coding History:
#    2008Jul02 DLLorenz Original coding.
#    2010Nov29 DLLorenz Conversion to R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Aug02 DLLorenz Start of bug fixes
#    2011Oct24 DLLorenz Tweaks for package
#    2011Oct24          This version.
#    

refLine <- function(horizontal, vertical, coefficients, # data, at least one must be used
                  Plot=list(name="", what='lines', type='solid',
                    width='standard', symbol='circle', filled=TRUE,
                    size=0.09, color='black'), # plot controls
                  current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                    xaxis.log=FALSE), # current plot parameters
                  xrange=c(NA,NA), yrange=c(NA,NA), # limit range of lines
                  log10=FALSE) { # was log base 10 used in any transforms
  ## add reference line
  ## arguments:
  ##   horizontal - horizontal lines 
  ##   vertical - vertical lines
  ##   coefficients - coefficients from a regression model
  ##   Plot - parameters of the plot, what is forced to lines
  ##   xrange - limit x-axis range of horizontal or regression line
  ##   yrange - limit y-axis range of vertical line
  ##   log10 - logical: was log base 10 transform used?
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
