#' Add a Smooth Line
#' 
#' Computes a smooth line from x and y data and adds the line to a graph.
#' 
#' 
#' @aliases addSmooth addSmooth.default addSmooth.list
#' @param x the x-axis data. For method \code{list}, x is a list that contains
#' components \code{x} and \code{y} and the \code{y} argument is not used.
#' Missing values are permitted and ignored in the smooth.
#' @param y the y-axis data. Missing values are permitted and ignored in the smooth.
#' @param Smooth the name of the smoothing function.
#' @param \dots additional parameters for the function names in \code{Smooth}.
#' @param Smooth.along the data along which the smoother is run. Must be either "x,"
#'which smooths \code{y} along \code{x} resulting in a horzontal line, or "y,"
#'which smooths \code{x} along \code{y} resulting in a vereical line.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return The current plot information.
#' @note If an error is generated from the smoother, then nothing is added to the
#'graph, an error is printed, the returned object contains missing values for
#'the data that should have been plotted, and the explanation is not updated.
#' @seealso \code{\link{addXY}}, \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' AA.pl <- xyPlot(X, Y)
#' addSmooth(AA.pl)
#' # For more details of addSmooth see
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' }
#' @export addSmooth
addSmooth <- function(x, y, # data
                      Smooth="loess.smooth", ..., # smoothing parameters
											Smooth.along="x",
                      Plot=list(name="", what="lines", type="solid",
                        width="standard", color="black"), # plot controls
                      current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                        xaxis.log=FALSE) # current plot parameters
                      ) { # right-axis labels and titles
	# Coding History:
	#    2010Mar23 DLLorenz Original coding.
	#    2011Apr07 DLLorenz Begin modifications for R
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Sep05 DLLorenz Made generic
	#    2013Aug20 DLLorenz Added tweaks to list method to facilitate scatter plot smooth
	#    2014May28 DLLorenz Added jitter to y to prevent failure if most of y is nearly linear
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  UseMethod("addSmooth")
}

#' @rdname addSmooth
#' @method addSmooth default
#' @export
addSmooth.default <- function(x, y, # data
                              Smooth="loess.smooth", ..., # smoothing paramaters
															Smooth.along="x",
                              Plot=list(name="", what="lines", type="solid",
                                width="standard", color="black"), # plot controls
                              current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                                xaxis.log=FALSE) # current plot parameters
                              ) { # right-axis labels and titles
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- transData(x, current$xaxis.log, FALSE,
                   current$xtrans, current$xtarg)
  ## Remove any missings
  good <- complete.cases(x, y)
  ## Protect against a primarily linear arrangement of y
  Plot$what <- "lines" # Force lines
  ## obey the request to change the smooting dir
  if(Smooth.along == "y") {
  	xtemp <- x
  	x <- y
  	y <- xtemp
  }
  ry <- diff(range(y[good]))/1000
  if(ry > 0) {
  	N <- sum(good)
  	smo <- try(do.call(Smooth, list(x=x[good], y=y[good] + runif(N, -ry, ry), ...)))
  	if(class(smo) == "try-error") {# continue with failure, but return missings
  		cat("Smoothed data not drawn, missing values returned\n")
  		smo <- list(x=NA_real_, y=NA_real_)
  		Plot$what <- "none" # Force nothing, so skip entry in explanation
  	}
  } else
  	smo <- list(x=range(x[good]), y=rep(y[good], length.out=2))
  Plot <- setPlot(Plot, name="", what="lines", type="solid",
                  width="standard", color="black") # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
  plotPars <- explan$current
  # recover original assignemtn of data if necessary
  if(Smooth.along == "y") {
  	xtemp <- smo$x
  	smo$x <- smo$y
  	smo$y <- xtemp
  }
  lines(smo$x, smo$y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  current$x <- smo$x
  current$y <- smo$y
  current$explanation <- explan
  invisible(current)
}

#' @rdname addSmooth
#' @method addSmooth list
#' @export
addSmooth.list <- function(x, y, # data
                           Smooth="loess.smooth", ..., # smoothing paramaters
													 Smooth.along="x",
                           Plot=list(name="", what="lines", type="solid",
                             width="standard", color="black"), # plot controls
                           current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                             xaxis.log=FALSE) # current plot parameters
                           ) { # right-axis labels and titles
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
    yrev <- xin$yaxis.rev
    xin$yaxis.rev <- FALSE
    ylog <- xin$yaxis.log
    xin$yaxis.log <- FALSE
    restore <- TRUE
    current <- xin
  } else if(!missing(current) && !is.null(xin$explanation)) {
  	xlog <- current$xaxis.log
  	current$xaxis.log <- FALSE
  	yrev <- current$yaxis.rev
  	current$yaxis.rev <- FALSE
  	ylog <- xin$yaxis.log
  	current$yaxis.log <- FALSE
  	restore <- TRUE
  }
  current <- addSmooth.default(x, y, Smooth=Smooth, ...,
  														 Smooth.along=Smooth.along,
                               Plot=Plot, current=current)
  if(restore) {
    current$xaxis.log <- xlog
    current$yaxis.rev <- yrev
    current$yaxis.log <- ylog
  }
  invisible(current)
}
