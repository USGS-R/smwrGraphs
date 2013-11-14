# Create a dot plot
#
# Coding History:
#    2011Jun23 DLLorenz Original coding.
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2012Aug28 DLLorenz dots for future methods
#    2012Sep27 DLLorenz Made generic
#    2013Jan04 DLLorenz Added multiPlot option
#    2013Apr09 DLLorenz Added setGD
#    2013Aug19 DLLorenz Added Date method
#

setGeneric("dotPlot", function(x, y, Plot=list(),
                               yaxis.orient="", yaxis.order="", yaxis.grid=TRUE,
                               xaxis.log=FALSE, xaxis.range=c(NA,NA),
                               ylabels="", xlabels=7, xtitle="", ytitle="",
                               caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("dotPlot"))

setMethod("dotPlot", signature("numeric"), # "ANY" ignored in last position
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.orient="table", yaxis.order="none",
         yaxis.grid=TRUE, # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels="full",
         xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle="", # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), jitter.y=TRUE, ...) { # margin controls
  ## arguments:
  ##   x - the x-axis data
  ##   y (character or factor vector) the y-axis data to plot
  ##   Plot - parameters of the plot
  ##   xaxis.log - log-transform the X axis
  ##   xaxis.range - set the range of the x-axis
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
  yaxis.orient <- match.arg(yaxis.orient, c("table", "grid"))
  if(length(yaxis.order) == 1)
    yaxis.order <- match.arg(yaxis.order, c("none", "ascending", "descending"))
  ylabels <- match.arg(ylabels, c("full", "abbreviate"))
  if(dev.cur() == 1)
    setGD("DotPlot")
  ## Quick fix for numeric Y
  if(is.numeric(y))
    y <- as.character(y)
  yax <- namePretty(y, orientation=yaxis.orient, order=yaxis.order,
                    label.abbr=ylabels == "abbreviate")
  ylev <- yax$labels
  y <- numericData(y, ylev)
  if(is.list(xlabels))
    xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=xlabels)
  
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
  par(mar=margin)
  ## Set up plot
  plot(x, y, type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  if(yaxis.grid) # draw light, gray lines
    segments(x0=xax$range[1], y0=y, x1=xax$range[2], y1=y,
             col="gray", lty=1, lwd=frameWt())
  ## Process plot control
  what=Plot$what[1]
  parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black")
  plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE) # part of returned data
  ## Draw each point
  if(jitter.y) {
    Grps <- unique(plot.info$name)
    if(length(Grps) == 1L)
      jitter.y <- 0
    else {
      Rng <- .4 - exp(-length(Grps)) # more-or-less works to expand range
      jitter.y <- seq(-Rng, Rng, length.out=length(Grps))
      names(jitter.y) <- Grps
      jitter.y <- jitter.y[plot.info$name] # one for each!
    }
  }
  points(x, y + jitter.y, type="p", pch=plot.info$pch, cex=plot.info$cex,
         col=plot.info$col, bg=ifelse(plot.info$filled, plot.info$col, 0))
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, yaxis.log=FALSE, yaxis.rev=FALSE,
                  yaxis.lev=ylev,
                  xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
                  yax=yax, xax=xax)))
}
)

setMethod("dotPlot", signature("Date"), # "ANY" ignored in last position
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.orient="table", yaxis.order="none",
         yaxis.grid=TRUE, # y-axis controls
         xaxis.log=FALSE, xaxis.range=range(x, na.rm=TRUE), # x-axis controls
         ylabels="full",
         xlabels="Auto", # labels
         xtitle="",
         ytitle="", # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), jitter.y=TRUE, ...) { # margin controls
  ## arguments:
  ##   x - the x-axis data
  ##   y (character or factor vector) the y-axis data to plot
  ##   Plot - parameters of the plot
  ##   xaxis.log - log-transform the X axis
  ##   xaxis.range - set the range of the x-axis
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
  yaxis.orient <- match.arg(yaxis.orient, c("table", "grid"))
  if(length(yaxis.order) == 1)
    yaxis.order <- match.arg(yaxis.order, c("none", "ascending", "descending"))
  ylabels <- match.arg(ylabels, c("full", "abbreviate"))
  if(dev.cur() == 1)
    setGD("DotPlot")
  ## Quick fix for numeric Y
  if(is.numeric(y))
    y <- as.character(y)
  yax <- namePretty(y, orientation=yaxis.orient, order=yaxis.order,
                    label.abbr=ylabels == "abbreviate")
  ylev <- yax$labels
  y <- numericData(y, ylev)
  xaxis.log=FALSE # Force the issue
  dax <- datePretty(xaxis.range, major=xlabels)
  x <- numericData(x)
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up plot
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  if(yaxis.grid) # draw light, gray lines
    segments(x0=dax$range[1], y0=y, x1=dax$range[2], y1=y,
             col="gray", lty=1, lwd=frameWt())
  ## Process plot control
  what=Plot$what[1]
  parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black")
  plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE) # part of returned data
  ## Draw each point
  if(jitter.y) {
    Grps <- unique(plot.info$name)
    if(length(Grps) == 1L)
      jitter.y <- 0
    else {
      Rng <- .4 - exp(-length(Grps)) # more-or-less works to expand range
      jitter.y <- seq(-Rng, Rng, length.out=length(Grps))
      names(jitter.y) <- Grps
      jitter.y <- jitter.y[plot.info$name] # one for each!
    }
  }
  points(x, y + jitter.y, type="p", pch=plot.info$pch, cex=plot.info$cex,
         col=plot.info$col, bg=ifelse(plot.info$filled, plot.info$col, 0))
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, yaxis.log=FALSE, yaxis.rev=FALSE,
                  yaxis.lev=ylev,
                  xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
                  yax=yax, xax=dax)))
}
)
