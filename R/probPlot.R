# Create a probability plot
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
#    2011Oct14 DLLorenz Bugfix in CF and tweaks
#    2012Mar15 DLLorenz Made generic
#    2012Sep17 DLLorenz Added xaxis.trans component to return object
#                        to flag that the x data are not transformed.
#    2013Apr09 DLLorenz Added setGD 
#    

probPlot <- function(x, truncate=NA,
                     FLIP=FALSE, distribution="normal",
                     alpha=0.4, # data specification
                     Plot=list(name="", what="points", type="solid",
                       width="standard", symbol="circle", filled=TRUE,
                       size=0.09, color="black"), # plot controls
                     yaxis.log=TRUE, yrange=c(NA, NA), # y-axis controls
                     ylabels=11,  xlabels=11, CDF=!RI, # labels
                     xtitle=ifelse(CDF, "Cumulative Probability",
                       "Exceedence Probability"),
                     RI=FALSE, RItitle="Recurrence Interval, in years",
                     ytitle=deparse(substitute(x)), # axis titles
                     caption="", # caption
                     margin=c(NA, NA, NA, NA), # margin control
                     ...) { # distribution parameters and method args
  ## build a probability plot
  ## arguments:
  ##   x - the data to plot
  ##   truncate - truncate the data at this value. If -Inf, no truncation;
  ##    if NA and yaxis.log=TRUE, truncate at 0 (equivalent to truncate=0);
  ##    if NA and yaxis.log=FALSE, no truncation (equivalent to truncate=-Inf);
  ##    any other numeric value,  truncate at that value.
  ##    Note that truncation implies a conditional probability adjustment
  ##   FLIP - plot traditional CDF if T,
  ##          plot as flipped data (largest values on left) if F.
  ##   distribution - the name of the desired function converting 
  ##                  from probabilities to coordinates
  ##   alpha - the alpha value of the function for computing plotting positions
  ##   yaxis.log - log-transform the Y axis
  ##   (x,y)labels - an estimate of the number of labels wanted
  ##   RI - add recurrence interval ticks and labels
  ##   CDF - label traditional cdf curve if T (increasing probabilities)
  ##         label with decreasing probabilities if F.
  ##   type - plot type, 'p' is points, 'l' is lines, etc.
  ##   pch - marker number
  ##   col - marker color
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   RItitle - x-axis 2 title if RI T
  ##   caption - the figure caption
  ## margin is used to control set up of plot:
  ## The margin is computed  and ticks drawn for any axis that has
  ## an unset margin of NA (default).
  ## If any are set 0 or positive, then it is assumed that the plot is set up
  ## and tick are drawn, but no labels
  ## If any are set negative, then it is assumed the the plot is set up
  ## and ticks are not drawn. if the value is less than -100, that is treated
  ## like -0.
  ##   ... parameters for the distribution function
  ##
  UseMethod("probPlot")
}

## default method
probPlot.default <- function(x, truncate=NA,
                             FLIP=FALSE, distribution="normal",
                             alpha=0.4, # data specification
                             Plot=list(name="", what="points", type="solid",
                               width="standard", symbol="circle", filled=TRUE,
                               size=0.09, color="black"), # plot controls
                             yaxis.log=TRUE, yrange=c(NA, NA), # y-axis controls
                             ylabels=11,  xlabels=11, CDF=!RI, # labels
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
  yax <- setAxis(x[plotthese], yrange, yaxis.log, FALSE, ylabels)
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
