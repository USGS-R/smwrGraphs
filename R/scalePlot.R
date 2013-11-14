# Plot x,y data, fixing the y/x scaling
#
# Coding History:
#    2008May13 DLLorenz Original coding.
#    2008Jun12 DLLorenz Start of revisions
#    2010Nov15 DLLorenz Begin modifications for R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2013Apr09 DLLorenz Added setGD 
#

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
  ## build a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
  ##   scale (numeric scalar) the y/x ratio
  ##   Plot - parameters of the plot
  ##   xaxis.log - log-transform the X axis
  ##   xaxis.range - set the range of the x-axis
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.rev - reverse the Y axis
  ##   yaxis.range - set the range of the y-axis
  ##   xlabels - an estimate of the number of labels wanted
  ##   ylabels - an estimate of the number of labels wanted
  ##     NOTE: either xlabels or ylabels can be a list of arguments to
  ##     linearPretty or logPretty to tweak output
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
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
