# Create an empirical cumulative distribution plot
#
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
#    

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
  ## Arguments:
  ##  x (numeric vector) the data to plot
  ##  group (group like vector to match x) the groups for each value in x
  ##  Plot (tagged list) -- what to plot
  ##  xaxis.log - log-transform the Y axis
  ##  (x,y)labels - an estimate of the number of labels wanted
  ##  xtitle - x-axis title
  ##  ytitle - y-axis title
  ##  caption - the figure caption
  ##  margin is used to control set up of plot:
  ## The margin is computed  and ticks drawn for any axis that has
  ## an unset margin of NA (default).
  ## If any are set 0 or positive, then it is assumed that the plot is set up
  ## and tick are drawn, but no labels
  ## If any are set negative, then it is assumed the the plot is set up
  ## and ticks are not drawn. if the value is less than -100, that is treated
  ## like -0.
  ##
  UseMethod("ecdfPlot")
}

## Code begins
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
  xax <- setAxis(x, xaxis.range, xaxis.log, FALSE, xlabels)
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
  yax <- linearPretty(c(0,1), labels=ylabels)
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
