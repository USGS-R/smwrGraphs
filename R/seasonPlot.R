# Plot data through season
#
# Coding History:
#    2012Feb02 DLLorenz Original coding from timePlot
#    2012Aug28 DLLorenz dots for future methods
#    2012Sep27 DLLorenz Made generic
#    2013Apr09 DLLorenz Added setGD 
#

setGeneric("seasonPlot", function(x, y, Plot=list(),
                                  yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
                                  ylabels=7, xlabels=7, xtitle="", ytitle="",
                                  caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("seasonPlot"))

setMethod("seasonPlot", signature("ANY", "numeric"), # can be Date, POSIXct, or decimal
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) {# margin controls
  ## build a seasonal-series plot
  ## arguments:
  ##   x - the time/date data
  ##   y - the data to plot
  ##   Plot - parameters of the plot
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.rev - reverse the Y axis
  ##   yaxis.range - set range in y-axis
  ##   xlabels - a description of the time-series labels
  ##   ylabels - Auto=full, abbrev, or letter
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
  ##
  ## set up the axes
  xtitle=xtitle
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("SeasonPlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  ## Until further notice, do dax here
  xlabels=match.arg(xlabels, c("Auto", "full", "abbrev", "letter"))
  xlabels <- switch(xlabels,
                    Auto=month.abb, full=month.name,
                    abbrev=month.abb, letter=c("J", "F", "M", "A", "M", "J",
                                        "J", "A", "S", "O", "N", "D"))
  dax <- list(ticks=c(0.0849, 0.1616, 0.2466, 0.3288, 0.4137, 0.4959,
                0.5808, 0.6657, 0.7479, 0.8329, 0.9151),
              finegrid=c(0.0849, 0.1616, 0.2466, 0.3288, 0.4137, 0.4959,
                0.5808, 0.6657, 0.7479, 0.8329, 0.9151),
              labels=xlabels,
              labelpos=c(0.04245, 0.12325, 0.20410, 0.28770, 0.37125, 0.45480,
                0.53835, 0.62325, 0.70680, 0.79040, 0.87400, 0.95755),
              range=c(0, 1), style="between")
  ## Convert to cyclic data
  x <- dectime(x)
  x <- x - trunc(x)
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## set graph
  plot(x, y, type='n', xlim=dax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  ## if multiplot
  if(any(sapply(Plot, length) > 1)) {
    parms <- setMultiPlot(Plot, length(x), name="", what='lines', type='solid',
                          width='standard', symbol='circle', filled=TRUE,
                          size=0.09, color='black')
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x
    plot.info$y <- y
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1], lwd=x$lwd[1], lty=x$lty[1],
             pch=x$pch[1], cex=x$cex[1], col=x$col[1], bg=x$col[1])
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    Plot <- setPlot(Plot, name="", what='points', type='solid',
                    width='standard', symbol='circle', filled=TRUE,
                    size=0.09, color='black') # force defaults if not set
    explan <- setExplan(Plot) # add info to set up explanation
    plotPars <- explan$current
    lines(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
          pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(list(x=x, y=y, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                 xaxis.log=FALSE, explanation=explan, margin=margin,
                 yax=yax, xax=dax))
}
)
