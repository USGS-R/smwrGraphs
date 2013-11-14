# Plot data through time
#
# Coding History:
#    2008May03 DLLorenz Original coding.
#    2008May06 DLLorenz Begin Tweaks
#    2011Jan05 DLLorenz Conversion to R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Jun16 DLLorenz Modified the y-axis set up
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2012Mar23 DLLorenz dots for future methods
#    2012Sep27 DLLorenz Made generic
#    2012Nov08 DLLorenz Added numeric and integer options for x
#    2012Nov14 DLLorenz Tweaks to new code
#    2013Apr09 DLLorenz Added setGD 
#

setGeneric("timePlot", function(x, y, Plot=list(),
                                yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
                                xaxis.range=range(x, na.rm=TRUE),
                                ylabels=7, xlabels=7, xtitle="", ytitle="",
                                caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("timePlot"))

setMethod("timePlot", signature("Date", "numeric"), # must be Date, 
function(x, y, # data
         Plot=list(name="", what="lines", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) {# margin controls
  ## build a time-series plot
  ## arguments:
  ##   x - the time/date data
  ##   y - the data to plot
  ##   Plot - parameters of the plot
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.rev - reverse the Y axis
  ##   yaxis.range - set range in y-axis
  ##   xaxis.range - set rtange of x-axis
  ##   xlabels - a description of the time-series labels
  ##   ylabels - an estimate of the number of labels wanted
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
  ##
  ## set up the axes
  xtitle=xtitle
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("DateTimePlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  dax <- datePretty(xaxis.range, major=xlabels)
  x <- numericData(x)
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## set graph
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## if multiplot
  if(any(sapply(Plot, length) > 1L)) {
    parms <- setMultiPlot(Plot, length(x), name="", what="lines", type="solid",
                          width="standard", symbol="circle", filled=TRUE,
                          size=0.09, color="black")
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x
    plot.info$y <- y
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1L], lwd=x$lwd[1L], lty=x$lty[1L],
             pch=x$pch[1L], cex=x$cex[1L], col=x$col[1L], bg=x$col[1L])
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    Plot <- setPlot(Plot, name="", what="lines", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
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


setMethod("timePlot", signature("POSIXt", "numeric"), # Almost the same as Date! 
function(x, y, # data
         Plot=list(name="", what="lines", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) {# margin controls
  ## Note this code is exactly the same as for class "Date," except that
  ## the call to datePretty, forces all labels to be between
  ## Any change to that function rerquires a change here!
  ##
  ## Set up the axes
  xtitle=xtitle
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("DateTimePlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  ## If the time span if between 1 and 3 days, force style to be at
  deltime <- difftime(xaxis.range[2L], xaxis.range[1L], units="days") 
  if(deltime > 1 && deltime < 3)
    dax <- datePretty(xaxis.range, major=xlabels, style="at")
  else
    dax <- datePretty(xaxis.range, major=xlabels, style="between")
  x <- numericData(x)
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## set graph
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## if multiplot
  if(any(sapply(Plot, length) > 1L)) {
    parms <- setMultiPlot(Plot, length(x), name="", what="lines", type="solid",
                          width="standard", symbol="circle", filled=TRUE,
                          size=0.09, color="black")
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x
    plot.info$y <- y
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1L], lwd=x$lwd[1L], lty=x$lty[1L],
             pch=x$pch[1L], cex=x$cex[1L], col=x$col[1L], bg=x$col[1L])
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    Plot <- setPlot(Plot, name="", what="lines", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
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

setMethod("timePlot", signature("numeric", "numeric"), # must be numeric 
function(x, y, # data
         Plot=list(name="", what="lines", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) {# margin controls
  ## build a time-series plot
  ## arguments:
  ##   x - the time/date data in decimal time
  ##   y - the data to plot
  ##   Plot - parameters of the plot
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.rev - reverse the Y axis
  ##   yaxis.range - set range in y-axis
  ##   xaxis.range - set rtange of x-axis
  ##   xlabels - a description of the time-series labels
  ##   ylabels - an estimate of the number of labels wanted
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
  ##
  ## set up the axes
  xtitle=xtitle
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("DateTimePlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  
  dax <- datePretty(xaxis.range, major=xlabels)
  x <- numericData(x)
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## set graph
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## if multiplot
  if(any(sapply(Plot, length) > 1L)) {
    parms <- setMultiPlot(Plot, length(x), name="", what="lines", type="solid",
                          width="standard", symbol="circle", filled=TRUE,
                          size=0.09, color="black")
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x
    plot.info$y <- y
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1L], lwd=x$lwd[1L], lty=x$lty[1L],
             pch=x$pch[1L], cex=x$cex[1L], col=x$col[1L], bg=x$col[1L])
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    Plot <- setPlot(Plot, name="", what="lines", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
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

setMethod("timePlot", signature("integer", "numeric"), # Treat as discrete values
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE) + c(-1, 1), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="Calendar Year",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), xlabels.rotate=FALSE, ...) {# margin controls
  ## Note this code is exactly the same as for class "Date," except that
  ## the call to datePretty, forces all labels to be between
  ## Any change to that function rerquires a change here!
  ##
  ## Set up the axes
  xtitle=xtitle
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("DateTimePlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  ## Use this to set user-specified hard limits
  hard <- !all(xaxis.range == range(x, na.rm=TRUE) + c(-1, 1))
  dax <- linearPretty(xaxis.range, hard=hard,
                      labels=xlabels, style="default")
  ## Set margins and controls
  if(xlabels.rotate) {
    botmar <- max(strwidth("2000", units="inches", family="USGS"))/par("cin")[2] + 1.2
    if(is.na(margin[1L]))
      margin[1L] <- pmax(3.2, botmar)
  }
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  if(xlabels.rotate)
    bot$angle <- 90 # Use this logic
  par(mar=margin)
  ## set graph
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## if multiplot
  if(any(sapply(Plot, length) > 1L)) {
    parms <- setMultiPlot(Plot, length(x), name="", what="lines", type="solid",
                          width="standard", symbol="circle", filled=TRUE,
                          size=0.09, color="black")
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x
    plot.info$y <- y
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1L], lwd=x$lwd[1L], lty=x$lty[1L],
             pch=x$pch[1L], cex=x$cex[1L], col=x$col[1L], bg=x$col[1L])
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    Plot <- setPlot(Plot, name="", what="lines", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
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

setMethod("timePlot", signature("difftime", "numeric"), # must be numeric 
function(x, y, # data
         Plot=list(name="", what="lines", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="Auto",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) {# margin controls
  ## build a time-series plot
  ## arguments:
  ##   x - the time difference data
  ##   y - the data to plot
  ##   Plot - parameters of the plot
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.rev - reverse the Y axis
  ##   yaxis.range - set range in y-axis
  ##   xaxis.range - set rtange of x-axis
  ##   xlabels - a description of the time-series labels
  ##   ylabels - an estimate of the number of labels wanted
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
  ##
  ## set up the axes
  if(xtitle == "Auto") {
    delt <- attr(x, "units")
    opts <- c(secs="Seconds", mins="Minutes", hours="Hours", days="Days", weeks="Weeks")
    xtitle <- paste("Time Difference, in ", opts[delt], sep="")
  }
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("TimeDiffPlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  
  dax <- timePretty(xaxis.range, labels=xlabels)
  x <- numericData(x)
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## set graph
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## if multiplot
  if(any(sapply(Plot, length) > 1L)) {
    parms <- setMultiPlot(Plot, length(x), name="", what="lines", type="solid",
                          width="standard", symbol="circle", filled=TRUE,
                          size=0.09, color="black")
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x
    plot.info$y <- y
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1L], lwd=x$lwd[1L], lty=x$lty[1L],
             pch=x$pch[1L], cex=x$cex[1L], col=x$col[1L], bg=x$col[1L])
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    Plot <- setPlot(Plot, name="", what="lines", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
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

