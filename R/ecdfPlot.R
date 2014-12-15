#' Empirical Distribution Plot
#' 
#' Produce a graph of the empirical distribution function of data.
#' 
#' 
#' @aliases ecdfPlot ecdfPlot.default
#' @param x the data to plot.
#' @param group create groups for \code{x}. Each group is plotted as a separate
#' line.
#' @param Plot control parameters of the plot.
#' @param xaxis.log log-transform the x axis?
#' @param xaxis.range set the range of the x-axis.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set the x-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @param \dots any additional arguments needed by specific methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{ecdfPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{probPlot}}
#' @keywords hplot
#' @export ecdfPlot
ecdfPlot <- function(x, group=NULL, # data specification
                     Plot=list(name="Auto", what="stairstep", type="solid",
                       width="standard", symbol="circle", filled=TRUE,
                       size=0.09, color="Auto"), # plot controls
                     xaxis.log=TRUE, xaxis.range=c(NA, NA), # x-axis controls
                     xlabels=11,  ylabels=5, # labels
                     ytitle="Cumulative Probability",
                     xtitle=deparse(substitute(x)), # axis titles
                     caption="", # caption
                     margin=c(NA, NA, NA, NA), ...) { # margin control
	# Coding History:
	#    2004Sep28 DLLorenz Original coding.
	#    2006Mar09 DLLorenz Fixed tics at every minor location
	#    2006Aug15 DLLorenz Modification to probpretty call and rename
	#    2008May08 DLLorenz Start of several revisions
	#    2010Nov29 DLLorenz Conversion to R
	#    2011Jan11 DLLorenz Added censoring option
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2012Mar15 DLLorenz Made generic
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  UseMethod("ecdfPlot")
}

#' @rdname ecdfPlot
#' @method ecdfPlot default
#' @export
ecdfPlot.default <- function(x, group=NULL, # data specification
                             Plot=list(name="Auto", what="stairstep", type="solid",
                               width="standard", symbol="circle", filled=TRUE,
                               size=0.09, color="Auto"), # plot controls
                             xaxis.log=TRUE, xaxis.range=c(NA, NA), # x-axis controls
                             xlabels=11,  ylabels=5, # labels
                             ytitle="Cumulative Probability",
                             xtitle=deparse(substitute(x)), # axis titles
                             caption="", # caption
                             margin=c(NA, NA, NA, NA), ...) { # margin control
  xtitle <- xtitle # needed to 'set' names
  ## Set up the axes and transform data
  if(dev.cur() == 1)
    setGD("ECDFPlot")
  xax <- setAxis(x, xaxis.range, xaxis.log, FALSE, xlabels, extend.range=FALSE)
  if(is.null(group)) {
    x <- sort(xax$data)
    x <- c(x[1], x) # Duplicate the first observation to be a riser
    N <- length(x)
    y <- seq(0, 1, length.out=N)
  }
  else {
    xgrp <- split(xax$data, group)
    xgrp <- lapply(xgrp, function(x) {
      x <- sort(x)
      x <- c(x[1], x)
      N <- length(x)
      y <- seq(0, 1, length.out=N)
      return(list(x=x, y=y))
    })
  }
  xax <- xax$dax
  yax <- linearPretty(c(0,1), hard=TRUE, labels=ylabels)
  ## Set up the plot
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  plot(range(x), c(0,1), type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## No group
  if(is.null(group)) {
    Plot <- setPlot(Plot, name="", what="stairstep", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
    explan <- setExplan(Plot) # add info to set up explanation
    plotPars <- explan$current
    points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
           pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col,
           bg=plotPars$col)
  }
  else {
    Plot <- setGroupPlot(Plot, Grps=length(xgrp), name=names(xgrp),
                         what="stairstep", type="solid",
                         width="standard", symbol="circle", filled=TRUE,
                         size=0.09, color="Auto")
    explan <- Plot$Explan
    plotPars <- Plot$current
    for(i in seq(length(xgrp)))
      points(xgrp[[i]]$x, xgrp[[i]]$y, type=plotPars$what[i],
             lwd=plotPars$lwd[i], lty=plotPars$lty[i],
             pch=plotPars$pch[i], cex=plotPars$cex[i], col=plotPars$col[i],
             bg=plotPars$col[i])
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(list(x=x, group=group, yaxis.log=FALSE, yaxis.rev=FALSE,
                 xaxis.log=xaxis.log, explanation=explan, margin=margin,
                 yax=yax, xax=xax))
}
