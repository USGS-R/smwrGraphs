#' Add a plot to a graph
#' 
#' Add points or lines to the current graph.
#' 
#' @name addXY
#' @rdname addXY
#' @aliases addXY addXY,ANY,numeric-method
#' addXY,numeric,character-method
#' @param x the x-axis data. Missing values are permitted, but result in breaks
#' in the plotted data.
#' @param y the y-axis data. Missing values are permitted, but result in breaks
#' in the plotted data.
#' @param Plot parameters of the plot.  See \code{\link{setPlot}} for a
#' description of the parameters.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @param right logical: set up new right axis?
#' @param right.log logical: log transform right axis?
#' @param right.rev logical: reverse right axis?
#' @param right.range set right-axis range.
#' @param right.labels set up right-axis labels.
#' @param right.title the right-axis title.
#' @param jitter.y adjust \code{y} values to reduce overlap for each group?
#' @param ... arguments for specific methods.
#' @return Information about the graph.
#' @docType methods
#' @section Methods: \describe{
#'
#'\item{signature(x ="ANY", y = "numeric"}{Any valid x-axis data and numeric y.}
#'\item{signature(x ="numeric", y = "character"}{Method to add to a dot plot;
#'the right-axis arguments are not valid.}
#'}
#' @keywords methods aplot
#' @exportMethod addXY
setGeneric("addXY", function(x, y, ...) standardGeneric("addXY")
#    2008Jun27 DLLorenz Original coding.
#    2010Nov20 DLLorenz Begin modifications for R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Jun17 DLLorenz fixed date conversion
#    2011Oct24 DLLorenz fixed call to renderY and tweak for package
#    2012Aug28 DLLorenz dots for future methods
#    2012Sep27 DLLorenz Made generic
#    2014Jun25 DLLorenz Converted to roxygen
)

#' @rdname addXY
setMethod("addXY", signature("ANY", "numeric"), 
function(x, y, # data
         Plot=list(name="", what='lines', type='solid',
           width='standard', symbol='circle', filled=TRUE,
           size=0.09, color='black'), # plot controls
         current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
           xaxis.log=FALSE), # current plot parameters
         right=FALSE, right.log=FALSE, right.rev=FALSE,
         right.range=c(NA,NA), ## right-axis controls
         right.labels=7, right.title='') { # right-axis labels and titles

  ##
  if(right) { # set up right axis
    rax <- setAxis(y, right.range, right.log, right.rev, right.labels)
    y <- rax$data
    rax <- rax$dax
    ## reset y-axis limits
    usr <- par('usr')
    usr[3:4] <- rax$range
    par(usr=usr)
    ## label right axis
    renderY(rax, left=list(ticks=FALSE, labels=FALSE), right=list(ticks=TRUE,
                                                         labels=TRUE),
            lefttitle='', righttitle=right.title)
    ## update current
    current$yaxis.log <- right.log
    current$yaxis.rev <- right.rev
  }
  else { # Use current y axis
    y <- numericData(y, lev=current$yaxis.lev)
    y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  }
  x <- numericData(x, lev=current$xaxis.lev) # Convert dates to consistent numeric
  x <- transData(x, current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  Plot <- setPlot(Plot, name="", what='lines', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black') # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
  plotPars <- explan$current
  lines(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  current$x <- x
  current$y <- y
  current$explanation <- explan
  invisible(current)
}
)

#' @rdname addXY
setMethod("addXY", signature("numeric", "character"), 
function(x, y, # data
				 Plot=list(name="", what='points', type='solid',
				 					width='standard', symbol='circle', filled=TRUE,
				 					size=0.09, color='black'), # plot controls
				 current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
				 						 xaxis.log=FALSE), # current plot parameters
				 jitter.y=FALSE) { # jitter y-values a bit

	##
	y <- numericData(y, lev=current$yaxis.lev)
	y <- transData(y, current$yaxis.log, current$yaxis.rev,
								 current$ytrans, current$ytarg)
	x <- numericData(x, lev=current$xaxis.lev) # Convert dates to consistent numeric
	x <- transData(x, current$xaxis.log, FALSE,
								 current$xtrans, current$xtarg)
	if(jitter.y) {
		jitter.y <- runif(length(y), -.1, .1)
	} else
		jitter.y <- 0
	Plot <- setPlot(Plot, name="", what='points', type='solid',
									width='standard', symbol='circle', filled=TRUE,
									size=0.09, color='black') # force defaults if not set
	explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
	plotPars <- explan$current
	points(x, y + jitter.y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
				 pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
	current$x <- x
	current$y <- y
	current$explanation <- explan
	invisible(current)
}
)
