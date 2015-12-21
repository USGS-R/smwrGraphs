#' X-Y Plot
#' 
#' Creates an x-y plot using arbitrary monotonic transforms for the axes.
#' 
#' @param x the x-axis data.
#' @param xtrans the transformation function for the x-axis.
#' @param xinv the inverse transformation for the x-axis.
#' @param xtargs additional arguments to \code{xtrans} and \code{xinv}, as a
#' list if necessary, NULL otherwise.
#' @param y the y-axis data.
#' @param ytrans the transformation function for the y-axis.
#' @param yinv the inverse transformation for the y-axis.
#' @param ytargs additional arguments to \code{ytrans} and \code{yinv}, as a
#' list if necessary, NULL otherwise.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param yaxis.range set the range of the y-axis.
#' @param xaxis.range set the range of the x-axis.
#' @param ylabels set up y-axis labels. See \code{\link{transPretty}} for
#' details.
#' @param xlabels set up x-axis labels. See \code{\link{transPretty}} for
#' details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{transPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{transPretty}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' X <- seq(.25, 9.75, by=.25)
#' setGD()
#' # The Box-Cox transform (power of 1.5)
#' # The labels represent the original values; the line represents the transformed value
#' transPlot(X, I, I, y=X, ytrans=boxCox, yinv=IboxCox, 
#'   ytargs=list(lambda=1.5, GM=1), Plot=list(what="lines"))
#' # For more details of transPlot see
#' demo(topic="MeasurementRating", package="smwrGraphs")
#' }
#' @export transPlot
transPlot <- function(x, xtrans, xinv, xtargs=NULL, y, ytrans,
                      yinv, ytargs=NULL, # data
                   Plot=list(name="", what='points', type='solid',
                     width='standard', symbol='circle', filled=TRUE,
                     size=0.09, color='black'), # plot controls
                   yaxis.range=c(NA,NA), # y-axis controls
                   xaxis.range=c(NA,NA), # x-axis controls
                   ylabels='Auto', xlabels='Auto', # labels
                   xtitle=deparse(substitute(x)),
                   ytitle=deparse(substitute(y)), # axis titles
                   caption='', # caption 
                   margin=c(NA, NA, NA, NA)) { # margin controls
	# Coding History:
	#    2010Nov29 DLLorenz Original coding.
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Oct24 DLLorenz Tweaks for package
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun27 DLLorenz Converted to roxygen
  ##
  ## create the plotting positions
  ## set up the axes
  if(dev.cur() == 1)
    setGD("TransPlot")
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  if(xlabels[1] == "Auto")
    xlabels=length(pretty(x))
  if(ylabels[1] == "Auto")
    ylabels=length(pretty(y))
  if(any(is.na(yaxis.range))) {
  	yax <- do.call("transPretty", c(list(x=y, labels=ylabels, func=ytrans, 
  																			 Ifunc=yinv), ytargs))
  } else
  	yax <- do.call("transPretty", c(list(x=yaxis.range, hard=TRUE, labels=ylabels, 
  																			 func=ytrans, Ifunc=yinv), ytargs))
  y <- do.call(ytrans, c(list(y), ytargs))
  if(any(is.na(xaxis.range))) {
  	xax <- do.call("transPretty", c(list(x=x, labels=xlabels, func=xtrans, 
  																			 Ifunc=xinv), xtargs))
  } else
  	xax <- do.call("transPretty", c(list(x=xaxis.range, hard=TRUE, labels=xlabels,
  																			 func=xtrans, Ifunc=xinv), xtargs))
  	x <- do.call(xtrans, c(list(x), xtargs))
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ##
  plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  Plot <- setPlot(Plot, name="", what='points', type='solid',
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
  invisible((list(x=x, y=y, yaxis.log=NA, yaxis.rev=FALSE, ytrans=ytrans,
                  ytargs=ytargs, xaxis.log=NA, xtrans=xtrans, xtargs=xtargs,
                  explanation=explan, margin=margin,
                  yax=yax, xax=xax)))
}
