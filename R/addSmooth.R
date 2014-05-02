# add smoothed x,y data 
#
# Coding History:
#    2010Mar23 DLLorenz Original coding.
#    2011Apr07 DLLorenz Begin modifications for R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Oct24 DLLorenz Tweaks for package
#    2012Sep05 DLLorenz Made generic
#    2013Aug20 DLLorenz Added tweaks to list method to facilitate scatter plot smooth.
#

addSmooth <- function(x, y, # data
                      Smooth="loess.smooth", ..., # smoothing paramaters
                      Plot=list(name="", what="lines", type="solid",
                        width="standard", color="black"), # plot controls
                      current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                        xaxis.log=FALSE) # current plot parameters
                      ) { # right-axis labels and titles
  ## add a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
  ##   Smooth - the name of the smoothing function
  ##   Plot - parameters of the plot
  ##   current - the current axis parameters
  UseMethod("addSmooth")
}

addSmooth.default <- function(x, y, # data
                              Smooth="loess.smooth", ..., # smoothing paramaters
                              Plot=list(name="", what="lines", type="solid",
                                width="standard", color="black"), # plot controls
                              current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                                xaxis.log=FALSE) # current plot parameters
                              ) { # right-axis labels and titles
  ## add a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
  ##   Smooth - the name of the smoothing function
  ##   Plot - parameters of the plot
  ##   current - the current axis parameters
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- transData(x, current$xaxis.log, FALSE,
                   current$xtrans, current$xtarg)
  ## Remove any missings
  good <- complete.cases(x, y)
  smo <- do.call(Smooth, list(x=x[good], y=y[good], ...))
  Plot$what <- "lines" # Force lines
  Plot <- setPlot(Plot, name="", what="lines", type="solid",
                  width="standard", color="black") # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
  plotPars <- explan$current
  lines(smo$x, smo$y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  current$x <- smo$x
  current$y <- smo$y
  current$explanation <- explan
  invisible(current)
}

addSmooth.list <- function(x, y, # data
                           Smooth="loess.smooth", ..., # smoothing paramaters
                           Plot=list(name="", what="lines", type="solid",
                             width="standard", color="black"), # plot controls
                           current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                             xaxis.log=FALSE) # current plot parameters
                           ) { # right-axis labels and titles
  ## add a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - a list containing x and y
  ##   y - not used
  ##   Smooth - the name of the smoothing function
  ##   Plot - parameters of the plot
  ##   current - the current axis parameters
  xin <- x
  if(is.null(x$y))
    stop("x is a list and must contain the component 'y'")
  y <- x$y
  if(is.null(x$x))
    stop("x is a list and must contain the component 'x'")
  x <- x$x
  if(length(x) != length(y))
    stop("the components 'x' and 'y' must have the same length")
  ## Try to maintain current if input is from a scatter plot
  restore <- FALSE
  if(missing(current) && !is.null(xin$explanation)) {
    xlog <- xin$xaxis.log
    xin$xaxis.log <- FALSE
    xrev <- xin$xaxis.rev
    xin$xaxis.rev <- FALSE
    ylog <- xin$yaxis.log
    xin$yaxis.log <- FALSE
    restore <- TRUE
    current <- xin
  }
  current <- addSmooth.default(x, y, Smooth=Smooth, ...,
                               Plot=Plot, current=current)
  if(restore) {
    current$xaxis.log <- xlog
    current$xaxis.rev <- xrev
    current$yaxis.log <- ylog
  }
  invisible(current)
}
