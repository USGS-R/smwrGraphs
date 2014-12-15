#' Series Plot
#' 
#' Produce a plot.
#' 
#' The argument \code{what} for \code{SeasonLine} must be either "lines" or
#' "vertical."  See \code{\link{monthplot}} for more information.\cr The
#' argument \code{what} for \code{SeasonPoint} can be set to "none" to suppress
#' drawing of that feature.
#' 
#' @aliases seriesPlot seriesPlot.default
#' @param x data that can be treated as a regularly-spaced time series. Missing
#' values are permitted, but result in missing seasons.
#' @param SeasonLine control parameters of the lines in the plot. See
#' \bold{Details}.
#' @param SeasonPoint control parameters of the points in the plot. See
#' \bold{Details}.
#' @param yaxis.log log-transform the y axis?
#' @param yaxis.range set the range of the y axis.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set the x-axis labels and number of seasons when \code{x} is
#' a simple numeric vector, may be a single numeric value indicating the number
#' of seasons in \code{x} or a vector of the names of the seasons. See
#' \code{\link{namePretty}} for details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @param \dots any additional arguments required for specific methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{seriesPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{monthplot}},
#' \code{\link{seasonPlot}}
#' @keywords hplot
#' @export seriesPlot
seriesPlot <- function(x, # data
                       SeasonLine=list(name="", what="vertical", color="black"),
                       SeasonPoint=list(name="", what="points", symbol="circle", 
                         filled=TRUE, size=0.09, color="black"), # plot controls
                       yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                       ylabels=7, xlabels, # labels
                       xtitle="",
                       ytitle="", # axis titles
                       caption="", # caption 
                       margin=c(NA, NA, NA, NA), ...) { # margin controls
  ## build a simple (single line or scatter) x-y plot
  ## arguments:
  ##   x - the time-series data
  ##   SeasonLine - parameters of the lines drawn for the data in each season
  ##   SeasonPoint - parameters of the points drawn for the data in each season
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.range - set the range of the y-axis
  ##   xlabels - the labels for the x-axis, required for x vector
  ##   ylabels - an estimate of the number of labels wanted
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
  ##   dots (dots) possibly required for some methods
  ##
  UseMethod("seriesPlot")
}

## Numeric data:
seriesPlot.default <- function(x, # data
                               SeasonLine=list(name="", what="vertical", color="black"),
                               SeasonPoint=list(name="", what="points", symbol="circle", 
                                 filled=TRUE, size=0.09, color="black"),
                               yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                               ylabels=7, xlabels=1, # labels, just 1 for x by default
                               xtitle="",
                               ytitle=deparse(substitute(x)), # axis titles
                               caption="", # caption 
                               margin=c(NA, NA, NA, NA), ...) { # margin controls
  ## Set reverse option for y-axis, needed as default
  ytitle <- ytitle
  yaxis.rev <- FALSE
  if(dev.cur() == 1)
    setGD("SeriesPlot")
  if(is.list(ylabels))
    yax <- c(list(data=x, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=x, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  if(length(xlabels) == 1)
    xlabels <- seq(xlabels)
  xlabels <- as.character(xlabels)
  xax <- namePretty(xlabels, orientation="grid")
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up the defaults for the lines and explanation:
  SeasonLine <- setDefaults(SeasonLine, name="", what="vertical", color="black")
  if(SeasonLine$what == "vertical")
    type <- "h"
  else if(SeasonLine$what == "lines")
    type <- "l"
  else {
    warning('invalid value for what; set to "verical" in SeasonLine')
    SeasonLine$what <- "vertical"
    type <- "h"
  }
  SeasonPoint <- setPlot(SeasonPoint, name="", what="points", symbol="circle", 
                             filled=TRUE, size=0.09, color="black")
  ##
  monthplot(y, labels=xlabels, type=type, xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="", box=FALSE)
  if(SeasonPoint$what == "none")
    explan <- setExplan(setPlot(list(), name="", what=SeasonLine$what, type="solid",
                                width="standard", symbol="circle", filled=TRUE,
                                size=0.09, SeasonLine$color))
  else { # Set the explanation and draw the points
    explan <- setExplan(SeasonPoint)
    N <- length(y)
    Nseas <- length(xlabels)
    xseq <- rep(seq(Nseas), N %/% Nseas) - 0.45 + seq(0,1, length.out=N) * 0.9
    ## Remove all data where at least one value is missing to avoid detached points
    xseas <- rep(seq(along=xlabels), length.out=N)
    yst <- tapply(y, xseas, function(xx) xx + 0*mean(xx))
    yst <- as.vector(do.call(rbind, yst))
    points(xseq, yst, pch=explan$current$pch, cex=explan$current$cex,
           col=explan$current$col, bg=explan$current$col)
  }
  box(lwd=frameWt())
  ## Label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  ## Pack y into individual series
  y <- split(y, rep(xlabels, length.out=length(y)))
  retval <- (list(y=y, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                  xaxis.log=FALSE, explanation=explan, margin=margin))
  ## Add lines if requested
  ## Suppress any log transforms for next section
  retval$yaxis.log <- retval$xaxis.log <- FALSE
  explan <- retval$explanation
  par(lwd=stdWt())
  ## recover the log-transforms and explanation if necessary
  retval$yaxis.log <- yaxis.log
  retval$yax <- yax
  retval$xax <- xax
  retval$explanation <- explan
  invisible(retval)
}
