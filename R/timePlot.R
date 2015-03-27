#' @title Time-series Plots
#' 
#' @description Produce a plot of time-series data.
#' 
#' @details For the \code{timePlot} methods where the time/date data are of class "Date,"
#'"POSIXt," or "numeric," the values for \code{xlabels} must be one of
#'"hours," "days," "months," "years," "water years," or "Auto," which will 
#'select an appropriate axis labeling scheme based on the time span of the data. 
#'May also be a list of valid arguments to \code{datePretty} for finer control.
#'
#'For the \code{timePlot} method where the time/date data are of class "integer," 
#'the value for \code{xlabels} must be one of "Auto," a number indicating the approximate
#'number of labels, or a list of valid arguments to \code{linearPretty} for finer control.
#'
#'For the \code{timePlot} method where the time/date data are of class "difftime," 
#'the value for \code{xlabels} must be one of "Auto" or a number indicating the approximate
#'number of labels.\cr
#'
#' For linear axes, the range can be set to virtually any pair of values. For 
#'log axes, the choice of range is more resticted---for less than one log-cycle,
#'powers of whole numbers can be used; from 1 to about 3 log cycles, the choces 
#'should be powers of 3 or 10; and for more than 3 log cycles, the range sould be 
#'expressed only in powers of 10.
#' 
#' @name timePlot
#' @rdname timePlot
#' @aliases timePlot timePlot,Date,numeric-method
#' timePlot,POSIXt,numeric-method timePlot,numeric,numeric-method
#' timePlot,integer,numeric-method timePlot,difftime,numeric-method
#' @param x the time/date data.
#' @param y the y-axis data.
#' @param Plot control parameters of the plot.
#' @param yaxis.log logical, if \code{TRUE}, then log-transform the y axis.
#' @param yaxis.rev logical, if \code{TRUE}, then reverse the y axis.
#' @param yaxis.range set the range of the y-axis. See \bold{Details}.
#' @param xaxis.range set the range of the x-axis. Set at January 1 through
#'December 31 for \code{seasonPlot}.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param xlabels set up x-axis labels. See \bold{Details} for details for
#'valid values.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param xlabels.rotate logical, if \code{TRUE}, then rotate the x-axis labels 
#' so that they are perpendicular to the x-axis.
#' @param ... arguments for specific methods.
#' @return Information about the graph.
#' @note The function \code{timePlot} produces a time-series plot. The function
#' \code{seasonPlot} produces a plot of the annual cycle. There is no function
#' in the smwrGraphs package that will automatically transform time/date data
#' to the correct seasonal value; use \code{dectime(x) - trunc(dectime(x))},
#' where \code{x} is the time/date variable.
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{signature(x = "Date", y = "numeric")}{ Create a
#' time-series plot for Date and numeric data. }
#' \item{signature(x ="POSIXt", y = "numeric")}{ Create a 
#' time-series plot for POSIXt and #' numeric data. } 
#' \item{signature(x = "numeric", y = "numeric")}{ Create a 
#' time-series plot for dates in decimal format and numeric data. }
#' \item{signature(x = "integer", y = "numeric")}{ Create a
#' time-series plot for annual summaries of numeric data. }
#' \item{signature(x = "difftime", y = "numeric")}{ Create a
#' time-series plot for difftime and numeric data. } }
#'
#' @seealso \code{\link{setPage}}, \code{\link{xyPlot}}, \code{\link{seasonPlot}}
#' @keywords methods hplot
#' @examples
#' \dontrun{
#' # See for examples of timePlot:
#' vignette(topic="DateAxisFormats", package="smwrGraphs")
#' vignette(topic="LineScatter", package="smwrGraphs")
#' demo(topic="AnnualFlowBarChart", package="smwrGraphs")
#' demo(topic="DurationHydrograph", package="smwrGraphs")
#' demo(topic="HydroPrecip", package="smwrGraphs")
#' demo(topic="RightAxisExample", package="smwrGraphs")
#' }
#' @exportMethod timePlot
setGeneric("timePlot", function(x, y, Plot=list(),
                                yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
                                xaxis.range=range(x, na.rm=TRUE),
                                ylabels=7, xlabels="Auto", xtitle="", ytitle="",
                                caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("timePlot")
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
					 #    2014Jun25 DLLorenz Converted to roxygen
)

#' @rdname timePlot
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
  if(class(xlabels) == "character") {
  	dax <- datePretty(xaxis.range, major=xlabels)
  } else # Should be list
  	dax <- do.call(datePretty, c(list(x=xaxis.range), xlabels))
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

#' @rdname timePlot
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
  ## If the time span is less than 3 days, force style to be at
  deltime <- difftime(xaxis.range[2L], xaxis.range[1L], units="days") 
  if(deltime < 3) {
  	if(class(xlabels) == "character") {
  		dax <- datePretty(xaxis.range, major=xlabels, style="at")
  	} else { # Should be list
  		if(is.null(xlabels$style))
  			xlabels$style <- "at"
  		dax <- do.call(datePretty, c(list(x=xaxis.range), xlabels))
  	}
  } else {
  	if(class(xlabels) == "character") {
  		dax <- datePretty(xaxis.range, major=xlabels)
  	} else # Should be list
  		dax <- do.call(datePretty, c(list(x=xaxis.range), xlabels))
  }
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

#' @rdname timePlot
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
  if(class(xaxis.range)[1L] == "numeric") # force to Date to set up axis
  	xaxis.range <- dectime2Date(xaxis.range)
  if(class(xlabels) == "character") {
  	dax <- datePretty(xaxis.range, major=xlabels)
  } else # Should be list
  	dax <- do.call(datePretty, c(list(x=xaxis.range), xlabels))
  # Now undo the axis limits 
  origin <- as.Date("1970-01-01")
  dax$ticks <- dectime(as.Date(dax$ticks, origin=origin))
  dax$finegrid <- dectime(as.Date(dax$finegrid, origin=origin))
  dax$labelpos <- dectime(as.Date(dax$labelpos, origin=origin))
  dax$range <- dectime(as.Date(dax$range, origin=origin))
  if(!is.null(dax$label2pos))
  	dax$label2pos <- dectime(as.Date(dax$label2pos, origin=origin))
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

#' @rdname timePlot
setMethod("timePlot", signature("integer", "numeric"), # Treat as discrete values
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE) + c(-1, 1), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
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

#' @rdname timePlot
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

