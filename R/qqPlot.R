# Create a q-q or q-normal plot
#
# Coding History:
#    2011Jun10 DLLorenz Original coding.
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2012Mar14 DLLorenz Conversion to generic function with methods
#    2012Mar15 DLLorenz Added option to suppress lines
#    2012Oct29 DLLorenz Tweaks for final
#    2013Apr09 DLLorenz Added setGD 
#

qqPlot <- function(x, y, # data
                   alpha=0.4,
                   Plot=list(name="Paired data quantiles", what="points", type="solid",
                     width="standard", symbol="circle", filled=TRUE,
                     size=0.09, color="black"),
                   LineRef=list(name="Line of best fit", what="lines", color="black"),
                   Line1.1=list(name="Line of equality", what="lines", color="gray"), # plot controls
                   yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                   xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
                   ylabels=7, xlabels=7, # labels
                   xtitle,
                   ytitle, # axis titles, missing out of necessity
                   caption="", # caption 
                   margin=c(NA, NA, NA, NA), ...) { # margin controls
  ## build a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data, or data to plot if no y-axis data to plot
  ##   y - the y-axis data to plot, if missing,
  ##   alpha - the plotting position calculator
  ##   Plot - parameters of the plot
  ##   LineRef - the fit of the data
  ##   line1.1 - a 1:1 line only for q-q plot
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
  ##   dots (dots) possibly required for some methods
  ##
  UseMethod("qqPlot")
}

## Numeric data:
qqPlot.default <- function(x, y, # data
                   alpha=0.4,
                   Plot=list(name="Paired data quantiles", what="points", type="solid",
                     width="standard", symbol="circle", filled=TRUE,
                     size=0.09, color="black"),
                   LineRef=list(name="Line of best fit", what="lines", color="black"),
                   Line1.1=list(name="Line of equality", what="lines", color="gray"), # plot controls
                   yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                   xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
                   ylabels=7, xlabels=7, # labels
                   xtitle,
                   ytitle, # axis titles, missing out of necessity
                   caption="", # caption 
                   margin=c(NA, NA, NA, NA), ...) { # margin controls
  ## create the plotting positions
  ## set up the data and axes
  if(dev.cur() == 1)
    setGD("QQPlot")
  if(missing(y)) { # makes it a q-normal plot
    Type <- "Q-N"
    if(missing(xtitle))
      xtitle <- "Normal Quantiles"
    if(missing(ytitle))
      ytitle <- deparse(substitute(x))
    y <- sort(x[!is.na(x)])
    x <- qnorm(ppoints(x, alpha))
  }
  else { # Q-Q plot, make lengths of x and y the same
    Type <- "Q-Q"
    if(missing(xtitle))
      xtitle <- deparse(substitute(x))
    if(missing(ytitle))
      ytitle <- deparse(substitute(y))
    ## Sort removes NAs
    x <- sort(x)
    y <- sort(y)
    lx <- length(x)
    ly <- length(y)
    if(lx != ly) { # fix it , see H&H chapter 2, section 2.2.5.1
      ahpla <- 1 - 2 * alpha
      if(lx > ly) {
        probs <- ((lx + ahpla) * (seq(ly) - alpha)/(ly+ahpla) + alpha - 1)/lx
        x <- quantile(x, probs=probs)
      }
      else { # ly > lx
        probs <- ((ly + ahpla) * (seq(lx) - alpha)/(lx+ahpla) + alpha - 1)/ly
        y <- quantile(y, probs=probs)
      }
    }
  }
  ## Set reverse option for y-axis, needed as default
  yaxis.rev <- FALSE
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
  ## Set up the defaults for the lines:
  LineRef <- setDefaults(LineRef, name="", what="lines", color="black")
  Line1.1 <- setDefaults(Line1.1, name="Line of equality", what="lines", color="gray")
  ##
  plot(x, y, type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  Plot <- setPlot(Plot, name="", what="points", type="solid",
                     width="standard", symbol="circle", filled=TRUE,
                     size=0.09, color="black") # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  ## Add lines if requested
  retval <- (list(x=x, y=y, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                  xaxis.log=xaxis.log, explanation=explan, margin=margin))
  ## Suppress any log treansforms for next section
  retval$yaxis.log <- retval$xaxis.log <- FALSE
  if(Type == "Q-N") {
    ## Add the reference line if requested
    if(LineRef$what != "none") {
      if(LineRef$name == "") # do not add to explanation
        refLine(coefficients=c(mean(y), sd(y)),
                Plot=LineRef, current=retval)
      else # do add to explanation
        retval <- refLine(coefficients=c(mean(y), sd(y)),
                          Plot=LineRef, current=retval)
    }
  }
  else {
    ## Add the reference line (line of organic correlation)
    if(LineRef$what != "none") {
      sl <- sd(y)/sd(x)
      zi <- mean(y) - sl*mean(x)
      if(LineRef$name == "") # do not add to explanation
        refLine(coefficients=c(zi, sl),
                Plot=LineRef, current=retval)
      else # do add to explanation
        retval <- refLine(coefficients=c(zi, sl),
                          Plot=LineRef, current=retval)
    }
    ## Add the 1:1 line
    if(Line1.1$what != "none")
      retval <- refLine(coefficients=c(0.0, 1.0),
                        Plot=Line1.1, current=retval)
  }
  ## recover the log-transforms if necessary
  retval$yaxis.log <- yaxis.log
  retval$xaxis.log <- xaxis.log
  retval$yax <- yax
  retval$xax <- xax
  invisible(retval)
}
