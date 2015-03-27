#' Probability Plot
#' 
#' Produce a probability plot.
#' 
#' Truncation of the data to plot (\code{x}) results in a conditional
#' probability plot. For any numeric value for \code{truncate}, the values in
#' \code{x} less than or equal to \code{truncate} are not plotted and the
#' remaining values are plotted at their conditional probability (the
#' probability computed with all values). The behavior for the default value
#' for \code{truncate} = NA, depends on \code{yaxis.log}. If \code{yaxis.log}
#' is TRUE, then \code{truncate} is treated as though it was 0; otherwise
#' \code{truncate} is treated as though it was \code{-Inf}, which results in no
#' truncation.
#' 
#' For linear axes, the range can be set to virtually any pair of values. For 
#'log axes, the choice of range is more resticted---for less than one log-cycle,
#'powers of whole numbers can be used; from 1 to about 3 log cycles, the choces 
#'should be powers of 3 or 10; and for more than 3 log cycles, the range sould be 
#'expressed only in powers of 10.
#' 
#' @aliases probPlot probPlot.default
#' @param x the data to plot. Missing values are allowed and ignored.
#' @param truncate truncate the data at the specified value. See
#' \bold{Details}.
#' @param FLIP if TRUE, then plot the cumulative distribution. Otherwise, plot
#' as flipped data (largest values on left).
#' @param distribution the name of the desired function converting from
#' probabilities to coordinates.
#' @param alpha the alpha value of the function for computing plotting
#' positions.
#' @param Plot control parameters of the plot.
#' @param yaxis.log logical, if \code{TRUE}, then log-transform the y axis.
#' @param yaxis.range set the range of the y axis. See \bold{Details}.
#' @param ylabels set the y-axis labels. See \code{\link{logPretty}} 
#'for \code{yaxis.log} set to \code{TRUE} or 
#'\code{\link{linearPretty}} for \code{yaxis.log} set to \\ceode{FALSE} for details.
#' @param xlabels set the x-axis labels. See \code{\link{probPretty}} for
#' details.
#' @param CDF logical, if \code{TRUE}, then label with increasing probabilities. Otherwise
#' label with decreasing probabilities.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param RI logical, if \code{TRUE}, then label the top axis with recurrence intervals.
#' If \code{RI} is set to \code{TRUE}, then \code{CDF} will be set to \code{FALSE}.
#' @param RItitle the top x-axis title if \code{RI} is \code{TRUE}.
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @param \dots parameters for the distribution function. If any parameter is
#' specified, then an attempt is made to draw the fit between the computed
#' distribution and the observed data.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{probPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{ecdfPlot}}, \code{\link{qqPlot}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of probPlot:
#' vignette(topic="ProbabilityPlots", package="smwrGraphs")
#' demo(topic="FlowDur-Measurements", package="smwrGraphs")
#' }
#' @export probPlot
probPlot <- function(x, truncate=NA,
                     FLIP=FALSE, distribution="normal",
                     alpha=0.4, # data specification
                     Plot=list(name="", what="points", type="solid",
                       width="standard", symbol="circle", filled=TRUE,
                       size=0.09, color="black"), # plot controls
                     yaxis.log=TRUE, yaxis.range=c(NA, NA), # y-axis controls
                     ylabels=11,  xlabels=11, CDF=!RI, # labels
                     xtitle=ifelse(CDF, "Cumulative Probability",
                       "Exceedence Probability"),
                     RI=FALSE, RItitle="Recurrence Interval, in years",
                     ytitle=deparse(substitute(x)), # axis titles
                     caption="", # caption
                     margin=c(NA, NA, NA, NA), # margin control
                     ...) { # distribution parameters and method args
	# Coding History:
	#    2004Sep28 DLLorenz Original coding.
	#    2006Mar09 DLLorenz Fixed tics at every minor location
	#    2006Aug15 DLLorenz Modification to probpretty call and rename
	#    2008May08 DLLorenz Start of several revisions
	#    2010Nov29 DLLorenz Conversion to R
	#    2011Jan11 DLLorenz Added censoring option
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Oct14 DLLorenz Bugfix in CF and tweaks
	#    2012Mar15 DLLorenz Made generic
	#    2012Sep17 DLLorenz Added xaxis.trans component to return object
	#                        to flag that the x data are not transformed.
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  UseMethod("probPlot")
}

#' @rdname probPlot
#' @method probPlot default
#' @export
probPlot.default <- function(x, truncate=NA,
                             FLIP=FALSE, distribution="normal",
                             alpha=0.4, # data specification
                             Plot=list(name="", what="points", type="solid",
                               width="standard", symbol="circle", filled=TRUE,
                               size=0.09, color="black"), # plot controls
                             yaxis.log=TRUE, yaxis.range=c(NA, NA), # y-axis controls
                             ylabels="Auto",  xlabels=11, CDF=!RI, # labels
                             xtitle=ifelse(CDF, "Cumulative Probability",
                               "Exceedence Probability"),
                             RI=FALSE, RItitle="Recurrence Interval, in years",
                             ytitle=deparse(substitute(x)), # axis titles
                             caption="", # caption
                             margin=c(NA, NA, NA, NA), # margin control
                             ...) { # distribution parameters and method args
  ## Need to 'set' names, etc
  ytitle <- ytitle
  CDF <- CDF
  xtitle <- xtitle
  x <- sort(x) # sort x and eliminate NAs
  pp <- ppoints(x, alpha)
  ## This truncatation is exactly the same as described in other sources
  ##  for the conditional probability adjustment
  if(is.na(truncate))
    truncate <- ifelse(yaxis.log, 0, -Inf)
  plotthese <- x > truncate
  ## set up the axes and transform data
  if(dev.cur() == 1)
    setGD("ProbabilityPlot")
  if(is.list(ylabels)) {
  	yax <- do.call(setAxis, c(list(data=x[plotthese], axis.range=yaxis.range, 
  																 axis.log=yaxis.log, axis.rev=FALSE), ylabels))
  } else 
  	yax <- setAxis(x[plotthese], yaxis.range, yaxis.log, FALSE, ylabels)
  x <- yax$data
  yax <- yax$dax
  ## Plot the full range of probability, but truncate pp to match x
  pax <- probPretty(pp, labels=xlabels, exceedence = !CDF, distribution=distribution, ...)
  pp <- pp[plotthese]
  ## set up the plot
  if(FLIP)
    pp <- 1 - pp
  ## get the dist function
  qdist <- getDist.fcn(distribution, 'q')
  xcoord <- qdist(pp, ...)
  ## set margins and controls
  if(RI && !CDF && is.na(margin[3]))
    margin[3] <- 3.3 # allow room for labels and title
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  
  par(mar=margin)
  plot(xcoord, x, type='n', xlim=pax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  Plot <- setPlot(Plot, name="", what='points', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black') # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  points(xcoord, x, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  box(lwd=frameWt())
  ## Label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(pax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  if(RI && !CDF) {
    pax$labels <- pax$RI # move to labels
    renderX(pax, bottom=list(ticks=FALSE, labels=FALSE), bottitle='',
            top=list(ticks=FALSE, labels=TRUE), toptitle=RItitle, caption='')
  }
  ## Fit line, if possible
  if(length(list(...)) > 0) { # if parameters specified try to plot line
    if(yaxis.log)
      abline(0, .4342945, col=plotPars$col, lwd=plotPars$lwd, lty=plotPars$lty)
    else
      abline(0, 1, col=plotPars$col, lwd=plotPars$lwd, lty=plotPars$lty)
  }
  ## Add non-null xaxis.trans to indicate that the x data are not
  ## transformed, but are as the original plotting positions.
  invisible(list(y=x, x=pp, yaxis.log=yaxis.log, yaxis.rev=FALSE,
                 xaxis.log=NA, explanation=explan, margin=margin,
                 xtrans=qdist, xtargs=list(...), xaxis.trans=FALSE,
                 yax=yax, xax=pax))
}
