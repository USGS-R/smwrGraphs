# Plot x,y data using aribtrary transforms
#
# Coding History:
#    2010Nov29 DLLorenz Original coding.
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2013Apr09 DLLorenz Added setGD 
#

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
  ## build a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the x-axis data
  ##   xtrans - the transformation function for x
  ##   xinv - the inverse transformation for x
  ##   xtargs - additional arguments to xtrans, as a list if necessary, NULL otherwise 
  ##   y - the y-axis data to plot
  ##   ytrans - the transformation function for y
  ##   yinv - the inverse transformation for y
  ##   ytargs - additional arguments to ytrans, as a list if necessary, NULL otherwise 
  ##   Plot - parameters of the plot
  ##   xaxis.range - set the range of the x-axis
  ##   yaxis.range - set the range of the y-axis
  ##   xlabels - an estimate of the number of labels wanted
  ##   ylabels - an estimate of the number of labels wanted
  ##     NOTE: either xlabels or ylabels can be a list of arguments to
  ##     transPretty to tweak output
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
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
  yax <- do.call("transPretty", c(list(x=y, labels=ylabels, func=ytrans, Ifunc=yinv), ytargs))
  y <- do.call(ytrans, c(list(y), ytargs))
  xax <- do.call("transPretty", c(list(x=x, labels=xlabels, func=xtrans, Ifunc=xinv), xtargs))
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
