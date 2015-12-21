#' Scale Plot
#' 
#' Produces a graph with a fixed aspect ratio for the x- and y-axes.
#' 
#' The \code{scale} argument sets the scaling ratio of the y-axis to the
#' x-axis. For latitude and longitude data, set the scale to
#' 1/cos(midlat/180*pi), where midlat is the midrange of the latitude.
#' 
#' For linear axes, the range can be set to virtually any pair of values. For 
#'log axes, the choice of range is more resticted---for less than one log-cycle,
#'powers of whole numbers can be used; from 1 to about 3 log cycles, the choces 
#'should be powers of 3 or 10; and for more than 3 log cycles, the range sould be 
#'expressed only in powers of 10.
#' 
#' @param x the x-axis data.
#' @param y the y-axis data.
#' @param scale the y/x ratio. See \bold{Details}.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param yaxis.log logical, if \code{TRUE}, then log-transform the y axis.
#' @param yaxis.rev logical, if \code{TRUE}, then reverse the y axis.
#' @param yaxis.range set the range of the y axis. See \bold{Details}.
#' @param xaxis.log logical, if \code{TRUE}, then log-transform the x axis.
#' @param xaxis.range set the range of the x axis. See \bold{Details}.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set the y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{scalePlot}.
#' @seealso \code{\link{setPage}}, \code{\link{xyPlot}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' setGD()
#' scalePlot(X, Y, Plot=list(what="points", size=0.05))
#' # For more details of scalePlot see
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @export scalePlot
scalePlot <- function(x, y, # data
                      scale=1, 
                      Plot=list(name="", what='lines', type='solid',
                        width='standard', symbol='circle', filled=TRUE,
                        size=0.09, color='black'), # plot controls
                      yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                      xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
                      ylabels=7, xlabels=7, # labels
                      xtitle=deparse(substitute(x)),
                      ytitle=deparse(substitute(y)), # axis titles
                      caption='', # caption 
                      margin=c(NA, NA, NA, NA)) { # margin controls
	# Coding History:
	#    2008May13 DLLorenz Original coding.
	#    2008Jun12 DLLorenz Start of revisions
	#    2010Nov15 DLLorenz Begin modifications for R
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Oct24 DLLorenz Tweaks for package
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  ## create the plotting positions
  ## set up the axes
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("ScaledPlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  if(is.list(xlabels))
    xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=xlabels)
  
  xax <- do.call("setAxis", xax)
  x <- xax$data
  xax <- xax$dax
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Initialize the plot and chnage the respective margin top or right
  ## to fix the scaling. Note, setting xaxs has no effect when asp is set.
  plot(x, y, type='n', xlim=xax$range, axes=FALSE,
       ylim=yax$range, ylab="", xlab="", asp=scale)
  xrange=xax$range[2] - xax$range[1]
  yrange=yax$range[2] - yax$range[1]
  mai <- par('mai')
  pin <- par('pin')
  if(pin[2]/pin[1] > yrange/xrange * scale) # too tall adjust top margin
    mai[3] <- mai[3] + (pin[2] - pin[1] * yrange/xrange * scale)
  else # to wide adjust right margin
    mai[4] <- mai[4] + (pin[1] - pin[2] * xrange/yrange / scale)
  ## Set margin and reset new so that a new page does not magically appear
  par(mai = mai, new = TRUE)
  ## manually adjust ranges and create the final plot
  xlim <- (xax$range - mean(xax$range))/1.08 + mean(xax$range)
  ylim <- (yax$range - mean(yax$range))/1.08 + mean(yax$range)
  plot(x, y, type='n', xlim=xlim, axes=FALSE, ylim=ylim, ylab="", xlab="",
       asp=scale)
  ## Continue as if nothing had happened
  Plot <- setPlot(Plot, name="", what='lines', type='solid',
                     width='standard', symbol='circle', filled=TRUE,
                     size=0.09, color='black') # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  lines(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                  xaxis.log=xaxis.log, explanation=explan, margin=margin,
                  yax=yax, xax=xax)))
}
