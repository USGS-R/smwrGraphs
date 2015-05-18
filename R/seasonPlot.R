#' Season Plot
#' 
#' Create a plot of data on a yearly cycle.
#' 
#' For \code{seasonPlot}, the value for \code{xlabels} must be one of "full," the full 
#'month names; "abbrev," abbreviations; or "letter," the first letter of the
#'month. The default is "Auto," which will select an appropriate labeling scheme.
#' 
#' @name seasonPlot
#' @rdname seasonPlot
#' @aliases seasonPlot seasonPlot,ANY,numeric-method
#' seasonPlot,character,numeric-method
#' @param x the x-coordinate data.
#' @param y the y-coordinate data.
#' @param Plot control parameters of the plot
#' @param yaxis.log logical, if \code{TRUE}, then log-transform the y axis.
#' @param yaxis.rev logical, if \code{TRUE}, then reverse the y axis.
#' @param yaxis.range set the range of the y-axis.
#' @param xaxis.range set the range of the x-axis. Must be one of "calendar,"
#' "water," or "climate" to set the type of year that is shown on the x-axis.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set up x-axis labels. See \bold{Details}.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param ... arguments for specific methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{seasonPlot}.
#' 
#' To add a plot to the graph created by \code{seasonPlot}, the x-axis data
#' must be expressed as decimal time relative to January 1. The function
#' \code{baseDay2decimal} can be used to convert data in the form of base day
#' to decimal time.
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{signature(x = "ANY", y = "numeric")}{ Create a seasonal
#' plot for any valid date data and numeric y data. }
#' 
#' \item{signature(x = "character", y = "numeric")}{ Create a
#' seasonal plot for date data in the form of month and day, like "Jan 01" or
#' "Jaunary 01." Typically used to plot daily mean values. } }
#' @seealso \code{\link{setPage}}, \code{\link{timePlot}}, 
#' \code{\link[smwrBase]{baseDay2decimal}}
#' @keywords methods hplot
#' @examples
#' \dontrun{
#' # the months function is in lubridate
#' X <- as.Date("2001-01-15") + months(0:11)
#' set.seed(1)
#' Y <- runif(12)
#' seasonPlot(X, Y)
#' # For more details of seasonPlot see
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @exportMethod seasonPlot
setGeneric("seasonPlot", function(x, y, Plot=list(),
                                  yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
																	xaxis.range="",
                                  ylabels=7, xlabels=7, xtitle="", ytitle="",
                                  caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("seasonPlot")
					 # Coding History:
					 #    2012Feb02 DLLorenz Original coding from timePlot
					 #    2012Aug28 DLLorenz dots for future methods
					 #    2012Sep27 DLLorenz Made generic
					 #    2013Apr09 DLLorenz Added setGD 
					 #    2014Jun26 DLLorenz Converted to roxygen
					 )

#' @rdname seasonPlot
setMethod("seasonPlot", signature("ANY", "numeric"), # can be Date, POSIXct, or decimal
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
				 xaxis.range=c("calendar", "water", "climate"),
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
  ytitle <- ytitle
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
  xaxis.range=match.arg(xaxis.range)
  xlabels <- switch(xlabels,
                    Auto=month.USGS, full=month.name,
                    abbrev=month.USGS, letter=c("J", "F", "M", "A", "M", "J",
                                        "J", "A", "S", "O", "N", "D"))
  if(xaxis.range == "calendar") {
  	dax <- list(ticks=c(0.0847, 0.1639, 0.2486, 0.3306, 0.4153, 0.4973,
  											0.5820, 0.6667, 0.7486, 0.8333, 0.9153),
  							finegrid=c(0.0847, 0.1639, 0.2486, 0.3306, 0.4153, 0.4973,
  												 0.5820, 0.6667, 0.7486, 0.8333, 0.9153),
  							labels=xlabels,
  							labelpos=c(0.04235, 0.12432, 0.20628, 0.28962, 0.37295, 0.45628,
  												 0.53962, 0.62432, 0.70765, 0.79098, 0.87432, 0.95765),
  							range=c(0, 1), style="between")
  	## Build the forward data transformation function
  	xtrans <- function(x) x %% 1
  } else if(xaxis.range == "water") {
  	dax <- list(ticks=c(0.0849, 0.1671, 0.2520, 0.3368, 0.4160, 0.5007,
  											0.5827, 0.6674, 0.7493, 0.8340, 0.9187),
  							finegrid=c(0.0849, 0.1671, 0.2520, 0.3368, 0.4160, 0.5007,
  												 0.5827, 0.6674, 0.7493, 0.8340, 0.9187),
  							labels= c(xlabels[10:12], xlabels[1:9]),
  							labelpos=c(0.04247, 0.12603, 0.20959, 0.29440, 0.37737, 0.45834,
  												 0.54167, 0.62501, 0.70834, 0.79167, 0.87637, 0.95967),
  							range=c(0, 1), style="between")
  	## Build the forward data transformation function
  	xtrans <- function(x) (x - 0.7485) %% 1
  } else { # Must be climate year
  	dax <- list(ticks=c(0.0822, 0.1671, 0.2493, 0.3342, 0.4192, 0.5014, 0.5863, 
  											0.6685, 0.7534, 0.8381, 0.9174),
  							finegrid=c(0.0822, 0.1671, 0.2493, 0.3342, 0.4192, 0.5014, 0.5863, 
  												 0.6685, 0.7534, 0.8381, 0.9174),
  							labels=c(xlabels[4:12], xlabels[1:3]),
  							labelpos=c(0.0411, 0.12466, 0.20822, 0.29178, 0.37671, 0.46027, 0.54384, 
  												 0.6274, 0.71096, 0.79577, 0.87774, 0.95971),
  							range=c(0, 1), style="between")
  	## Build the forward data transformation function
  	xtrans <- function(x) (x - 0.248) %% 1
  }
  ## Convert to cyclic data
  x <- dectime(x)
  x <- xtrans(x)
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
                 xaxis.log=NA, xtrans=xtrans, xtargs=NULL, 
  							 explanation=explan, margin=margin,
                 yax=yax, xax=dax))
}
)

#' @rdname seasonPlot
setMethod("seasonPlot", signature("character", "numeric"), 
function(x, y, # data
				 Plot=list(name="", what="lines", type="solid",
				 					width="standard", symbol="circle", filled=TRUE,
				 					size=0.09, color="black"), # plot controls
				 yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA, NA), # y-axis controls
				 xaxis.range=c("calendar", "water", "climate"),
				 ylabels=7, xlabels="Auto", # labels
				 xtitle="",
				 ytitle=deparse(substitute(y)), # axis titles
				 caption="", # caption 
				 margin=c(NA, NA, NA, NA), ...) {# margin controls
	## build a seasonal-series plot
	##
	## set up the x-axis data and pass to the main driver
	ytitle <- ytitle
	## Must use a leap year to convert Feb 29 correctly
	x <- as.Date(paste("2000 ", x, sep=""), format="%Y %b %d")
	seasonPlot(x=x, y=y, Plot=Plot, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
						 yaxis.range=yaxis.range, xaxis.range=xaxis.range,
						 ylabels=ylabels, xlabels=xlabels, xtitle=xtitle, ytitle=ytitle,
						 caption=caption, margin=margin, ...)
}
)
