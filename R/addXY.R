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
#' @param new.axis character:  indicating which new axis to set up. Must be either "right," "top,"
#' or "none," which indicates that the existing axes be used (default).
#' @param new.log logical: log transform new axis?
#' @param new.rev logical: reverse new axis?
#' @param new.range set new-axis range.
#' @param new.labels set up new-axis labels.
#' @param new.title the new-axis title.
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
         new.axis="none", new.log=FALSE, new.rev=FALSE,
         new.range=c(NA,NA), ## right/top-axis controls
         new.labels=7, new.title='') { # right/top-axis labels and titles

  ##
  new.axis <- match.arg(new.axis, c("none","right","top"))
  
  if(new.axis == "right") { # set up right axis
    rax <- setAxis(y, new.range, new.log, new.rev, new.labels)
    y <- rax$data
    rax <- rax$dax
    ## reset y-axis limits
    usr <- par('usr')
    usr[3:4] <- rax$range
    par(usr=usr)
    ## label right axis
    renderY(rax, left=list(ticks=FALSE, labels=FALSE), right=list(ticks=TRUE,
                                                         labels=TRUE),
            lefttitle='', righttitle=new.title)
    ## update current
    current$yaxis.log <- new.log
    current$yaxis.rev <- new.rev
  } else if (new.axis == "top"){
    rax <- setAxis(x, new.range, new.log, new.rev, new.labels)
    x <- rax$data
    rax <- rax$dax
    ## reset x-axis limits
    usr <- par('usr')
    usr[1:2] <- rax$range
    par(usr=usr)
    renderX(rax, bottom=list(ticks=FALSE, labels=FALSE), top=list(ticks=TRUE,
                                                                  labels=TRUE),
            bottitle='', toptitle=new.title)
    ## update current, no reverse x-axis
    current$xaxis.log <- new.log
    if(new.rev)
    	warning("x-axis cannot be reversed")
  }
  ## Transform the axis
  y <- numericData(y, lev=current$yaxis.lev)
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
  							 current$ytrans, current$ytarg)
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
