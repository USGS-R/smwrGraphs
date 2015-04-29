#' Histogram
#' 
#' Create either a frequency or density histogram.
#' 
#' @details To set the x-axis range, you must specify numeric breaks that span the
#'complete range of \code{x}.
#'
#' The components of \code{Hist}:
#'\describe{
#'\item{"type"}{The type of the histogram. Must be one of "frequency" for actual counts
#'in the bin, "density" for density in each bin, or "relative frequency" for percent in each bin.}
#'\item{"fill"}{Logical value, \code{TRUE} means each bin will be shaded with \code{fill.color}.}
#'\item{"boundary"}{Defines how values tied to bin limit boundaires are handled. If "upper," 
#'then the bin limit boundary is the upper limit of the range and values tied to that value 
#'are placed in the bin corresponding to the upper limit of the boundary. If "lower," then
#'the bijn limit is the lower limit of the bin.}
#'\item{line.color}{The color of the lines around the bins.}
#'\item{fill.color}{The color the bins.}
#'}
#'
#' @aliases histGram histGram.default
#' @param x a numeric vector to create the histogram
#' @param breaks any valid value for \code{\link{hist}}. See \bold{Details}.
#' @param Hist Controls for the histogram.
#' @param yaxis.range set the range for the y axis, the first value must be 0.
#' @param ylabels the approximate number of labels for the y axis.
#' @param xlabels the approximate number of labels for the x axis. The default value,
#'"Auto" sets labels that are aligned with the breaks.
#' @param xtitle x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption), 
#'"Frequency" for a frequency histogram,
#'"Density" for a density histogram.
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param \dots additional arguments for other methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{histGram}.
#' @seealso \code{\link{ecdfPlot}}
#' @references #'Helsel, D.R., and Hirsch, R.M., 2002, Statistical methods in water resources: 
#'U.S. Geological Survey Techniques of Water-Resources Investigations, book 4, 
#'chap. A3, 522 p.
#' @keywords hplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' Xbig <- rnorm(100)
#' histGram(Xbig, breaks=seq(-3, 3, by=.5), Hist=list(type="density"))
#' # For more details of histGram see
#' vignette(topic="ProbabilityPlots", package="smwrGraphs")
#' }
#' @export histGram
histGram <- function(x, breaks="Sturges", # data specs
                     Hist=list(), # plot controls
                     yaxis.range=c(NA,NA), # y-axis controls
                     ylabels=7, xlabels="Auto", # labels
                     xtitle="",
                     ytitle="Auto", # axis titles
                     caption="",# caption
                     margin=c(NA, NA, NA, NA), ...) { # margin control
	# Coding history:
	#    2012Oct16 DLLorenz Initial coding and begin edits
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun26 DLLorenz Converted to roxygen
	#
  UseMethod("histGram")
}

#' @rdname histGram
#' @method histGram default
#' @export
histGram.default <- function(x, breaks="Sturges", # data specs
                             Hist=list(type="frequency", fill=FALSE, boundary="lower",
                               line.color="black", fill.color="gray80"), # plot controls
                             yaxis.range=c(NA,NA), # y-axis controls
                             ylabels=7, xlabels=7, # labels
                             xtitle=deparse(substitute(x)),
                             ytitle="Auto", # axis titles
                             caption="",# caption
                             margin=c(NA, NA, NA, NA), ...) { 
  ## Note, to set the x-axis range, set the specific breaks
  xtitle <- xtitle
  if(dev.cur() == 1)
    setGD("Histogram")
  ## Need to figure out how to process xaxis.log, maybe just log(x)
  ## But then need to find nice labels for x too!
  Hist <- setDefaults(Hist, type="frequency", fill=FALSE, boundary="lower",
                      line.color="black", fill.color="gray80")
  Hist$type <- match.arg(Hist$type, c("frequency", "density", "relative frequency"))
  if(Hist$type == "relative frequency") {
  	tweakY <- TRUE
  	Hist$type <- "density"
  } else {
  	tweakY <- FALSE
  }
  Hist$boundary <- match.arg(Hist$boundary, c("lower", "upper"))
  ret <- hist(x, breaks, right=Hist$boundary == "upper", plot=FALSE)
  if(xlabels == "Auto") {
  	xlabels <- ret$breaks
  }
  xax <- linearPretty(range(ret$breaks), TRUE, xlabels)
  if(Hist$type == "frequency") {
    if(any(is.na(yaxis.range)))
      yax <- linearPretty(c(0, max(ret$counts)), FALSE, ylabels, extend.range=FALSE)
    else
      yax <- linearPretty(yaxis.range, TRUE, ylabels)
    freq <- TRUE
    if(ytitle == "Auto")
      ytitle <- "Frequency"
  }
  else {
    if(any(is.na(yaxis.range)))
      yax <- linearPretty(c(0, max(ret$density)), FALSE, ylabels, extend.range=FALSE)
    else
      yax <- linearPretty(yaxis.range, TRUE, ylabels)
    freq <- FALSE
    if(ytitle == "Auto")
      ytitle <- "Density"
    # Adjust labels and title
    if(tweakY) {
    	tweakY <- unique(diff(ret$breaks))
    	if(length(tweakY) != 1L) {
    		stop("cannot compute relative frequency if breaks are not uniform")
    	}
    	if(ytitle == "Density") {
    		ytitle <- "Relative Frequency, in percent"
    	}
    	yax$labels <- format(yax$labelpos * tweakY * 100)
    }
  }
  ## Set margins, controls in calls to render
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  par(mar=margin)
  fillcolor <- if(Hist$fill) Hist$fill.color
  plot(ret, freq=freq, col=fillcolor, border=Hist$line.color,
       main="", sub="", xlab="", ylab="", xlim=xax$range, ylim=yax$range,
       axes=FALSE, labels=FALSE)
  ## Set up the explanation
  Col <- if(Hist$fill) Hist$fill.color else "white"
  Plot <- setPlot(list(), name=ytitle, what='points', type='solid',
                  width='standard', symbol='none', filled=TRUE,
                  size=0.09, color='black', area.color=Col,
                  area.border=Hist$line.color) # force defaults if not set
  explan <- setExplan(Plot, old=list()) # add info to set up explanation
  ## Label the axes No box! and need extend=TRUE
  renderY(yax, lefttitle=ytitle, left=list(ticks = TRUE, labels = TRUE,
                                   grid = FALSE, finegrid = FALSE,
                                   extend=TRUE),
          right=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
            finegrid = FALSE))
  renderX(xax, bottitle=xtitle, bottom=list(ticks = TRUE, labels = TRUE,
                                  grid = FALSE, finegrid = FALSE,
                                  extend = TRUE),
          top=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
            finegrid = FALSE), caption=caption)
  ## Need to complete the info needed in ret
  invisible(list(hist=ret, yaxis.log=FALSE, yaxis.rev=FALSE,
                  xaxis.log=FALSE, explanation=explan, margin=margin,
                  yax=yax, xax=xax))
}
