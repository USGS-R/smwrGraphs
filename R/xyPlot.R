# Plot x,y data 
#
# Coding History:
#    2008May13 DLLorenz Original coding.
#    2008Jun12 DLLorenz Start of revisions
#    2010Nov15 DLLorenz Begin modifications for R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2012Jan10 DLLorenz Allow labels=0 to suppress ticks and labels
#    2012Mar23 DLLorenz dots for future methods
#    2012Sep27 DLLorenz Made S4 generic--Note for generic methods that might ever 
#                       require or use deparse(substitute(ARG)), those arguments 
#                       must be in the call to setGeneric and ...; any other 
#                       overriding defaults can be set in the method function!
#    2012Nov14 DLLorenz Added signature "factor", "numeric"
#    2012Nov15 DLLorenz Changed all defaults to "points"
#    2013Apr09 DLLorenz Added setGD 
#

setGeneric("xyPlot", function(x, y, Plot=list(),
                              yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
                              xaxis.log=FALSE, xaxis.range=c(NA,NA),
                              ylabels=7, xlabels=7, xtitle="", ytitle="",
                              caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("xyPlot"))

setMethod("xyPlot", signature("numeric", "numeric"),
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA), # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels=7, xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) { # margin controls
  ## build a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
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
  xtitle <- xtitle # needed to 'set' names
  ytitle <- ytitle
  ylabel0 <- FALSE
  if(dev.cur() == 1)
    setGD("XYPlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else {
    if(is.numeric(ylabels) && length(ylabels) == 1 && ylabels == 0) {
      ylabel0 <- TRUE
      yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev, axis.labels=2)
    }
    else
      yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev, axis.labels=ylabels)
  }
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  xlabel0 <- FALSE
  if(is.list(xlabels))
    xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else {
    if(is.numeric(xlabels) && length(xlabels) == 1 && xlabels == 0) {
      xlabel0 <- TRUE
      xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=2)
    }
    else
      xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE, axis.labels=xlabels)
  }
  xax <- do.call("setAxis", xax)
  x <- xax$data
  xax <- xax$dax
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  ## Suppress ticks and labels if requested
  if(ylabel0)
    left$ticks <- left$labels <- right$ticks <- right$labels <- FALSE
  if(xlabel0)
    bot$ticks <- bot$labels <- top$ticks <- top$labels <- FALSE
  par(mar=margin)
  ##
  plot(x, y, type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  Plot <- setPlot(Plot, name="", what="points", type="solid",
                     width="standard", symbol="circle", filled=TRUE,
                     size=0.09, color="black") # force defaults if not set
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
)

setMethod("xyPlot", signature("factor", "numeric"),
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA), # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), xlabels.rotate=FALSE, ...) { # margin controls
  ## build a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
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
  ## Set up the axes
  xtitle <- xtitle # needed to 'set' names
  ytitle <- ytitle
  ylabel0 <- FALSE
  if(dev.cur() == 1)
    setGD("XYPlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else {
    if(is.numeric(ylabels) && length(ylabels) == 1 && ylabels == 0) {
      ylabel0 <- TRUE
      yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev, axis.labels=2)
    }
    else
      yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev, axis.labels=ylabels)
  }
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  ## Set up for rotated axis labels
  if(xlabels[1] != "Auto") 
    parnames <- xlabels
  else
    parnames <- levels(x)
  if(xlabels.rotate) {
    botmar <- max(strwidth(parnames, units="inches", family="USGS"))/par("cin")[2] + 2.2
    if(is.na(margin[1]))
      margin[1] <- pmax(3.2, botmar)
  }
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  ## Continue with x-axis
  xax <- namePretty(parnames, orientation="grid", offset=1)
  bot$ticks <- top$ticks <- FALSE
  x <- as.numeric(x)
  ## Suppress y-axis ticks and labels if requested
  if(ylabel0)
    left$ticks <- left$labels <- right$ticks <- right$labels <- FALSE
  par(mar=margin)
  ##
  plot(x, y, type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  Plot <- setPlot(Plot, name="", what="points", type="solid",
                     width="standard", symbol="circle", filled=TRUE,
                     size=0.09, color="black") # force defaults if not set
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
)
