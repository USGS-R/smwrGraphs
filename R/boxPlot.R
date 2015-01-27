#' Box Plot
#' 
#' Produces a truncated, simple, Tukey, or extended box plot.
#' 
#' If group is numeric, then the boxes will be plotted along a continuous
#' numeric axis. Otherwise the x-axis will be discrete groups.\cr \code{Box} is
#' a list with these components: \describe{ \item{"type"}{the type of boxtplot:
#' "simple" the whiskers extend to the minimum and maximum of the data,
#' "truncated" the whisker extend to percentiles defined by \code{truncated},
#' "tukey" the standard "Tukey" boxplot, and "extended" the whisker extend to
#' percentiles defined by \code{truncated} and values outside of that range are
#' shown;} \item{"show.counts"}{show the number of observations used to compute
#' the boxplot statistics;} \item{"nobox"}{only individual values are shown if
#' the number of observations is less than or equal to this value;}
#' \item{"width"}{the width of the box, in inches;} \item{"fill"}{The color of
#' the filled box or "none" for no fill;} \item{"truncated"}{the percentiles to
#' use for the truncated boxplot.} }
#' 
#' @aliases boxPlot boxPlot.list boxPlot.data.frame boxPlot.numeric
#' @param \dots the data to plot.
#' @param group create groups of a single numeric vector. Invalid for any kind
#' of object other than numeric.
#' @param Box control parameters for the box. See \bold{Details}.
#' @param yaxis.log logical: log transform y axis?
#' @param yaxis.range set y-axis range.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#'details; the value for \code{ylabels} can be set to an valid value for the
#'\code{label} argument in \code{linearPretty} or a tagged list with values
#'spcified for the arguments in \code{linearPretty}.
#' @param xlabels set up x-axis labels. Must be either "Auto" or a character 
#'vector of the x-axis labels.
#' @param xlabels.rotate rotate x-axis labels 90 degrees?
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption
#' @param margin set up the plot area margins.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{boxPlot}.\cr If \code{yaxis.log} is set to
#' \code{TRUE}, then the quartiles and interquartile range are computed from
#' the log-transformed values rather then the untransformed values, which is
#' common for other box plots. Those computations are in agreement with the box
#' plots generated in the QWGRAPH component of the QWDATA module in the
#' National Water Information System (NWIS) described by Dennis Helsel's 1989
#' Branch of Systems Analysis Technical Memorandum No. 89.01, available online
#' at \url{http://water.usgs.gov/admin/memo/BSA/BSA89.01.pdf}. Those
#' computations have a significant effect on the appearance the whiskers and
#' outside values of the Tukey box plot and are motivated by the general
#' assumption of a log-normal distribution for most water-quality constituents.
#' @seealso \code{\link{setPage}}, \code{\link{dotPlot}}
#' @keywords hplot
#' @export boxPlot
boxPlot <- function(..., group=NULL, # data
                    Box=list(type="truncated", show.counts=TRUE, 
                      nobox=5, width="Auto", fill="none",
                      truncated=c(10,90)), # b&w controls, color always black!
                    yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                    ylabels="Auto", xlabels="Auto", xlabels.rotate=FALSE, # labels
                    xtitle="", ytitle="",  caption="", # axis titles and caption
                    margin=c(NA,NA,NA,NA)) { # margin control
	# Coding history:
	#    2008May15 DLLorenz Modified from usgsboxplot to produce 'pub' ready plots
	#    2010Dec16 DLLorenz Conversion to R
	#    2011Jun25 DLLorenz Interface update and fix rotation of x-labels
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Oct24 DLLorenz Bug fix and Tweaks for package
	#    2011Dec16 DLLorenz Conversion to generic function with methods
	#    2012Feb24 DLLorenz Begin update to new pub standards
	#    2012Apr20 DLLorenz Created fcn to compute quartiles, not hinges
	#    2012Aug31 DLLorenz Big fixes and tweaks
	#    2012Sep18 DLLorenz Added fill option to box.
	#    2012Nov01 DLLorenz Bug fix on missing values
	#    2013Jan13 DLLorenz Change drop censor args in Box (retain in defaults)
	#    2013Apr10 DLLorenz Added setGD
	#    2014Jan28 DLLorenz Added match.arg for Box
	#    2014Apr23 DLLorenz Bug fix for NAs
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  UseMethod("boxPlot")
}

#' @rdname boxPlot
#' @export
#' @method boxPlot numeric
boxPlot.numeric <- function(..., group=NULL, # data
                            Box=list(type="truncated", show.counts=TRUE,
                              nobox=5, width="Auto", fill="none",
                              truncated=c(10,90)), # b&w controls
                            yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                            ylabels="Auto", xlabels="Auto",
                            xlabels.rotate=FALSE, # labels
                            xtitle="", ytitle="",  caption="", # axis titles and caption
                            margin=c(NA,NA,NA,NA)) { # margin control
  ## Process data to plot, all must be numeric. If only 1, then can be grouped
  dots <- list(...)
  if(!is.null(group)) {
    if(length(dots) == 1) { # split the data, names derived from group
      if(!is.numeric(group))
        group <- recode(group, "", " ")
      dots <- split(dots[[1]], group)
      Ndots <- length(dots)
      if(is.numeric(group)) { # set xrange by data
        xrange <- range(group)
        if(Ndots > 1)
          meanspacing <- diff(xrange)/(Ndots-1)
        else
          meanspacing <- 1.0
        if(!is.numeric(xlabels))
          xlabels <- 7 # must force to numeric
        xrange <- xrange + c(-meanspacing, meanspacing)/2
        xtoplot <- as.double(names(dots))
      }
      else {
        xrange <- c(0, Ndots + 1)
        xtoplot <- seq(Ndots)
      } # end of xrange logic
    }
    else
      stop("Multiple numeric vectors cannot be grouped by boxPlot")
  }
  else {
    xrange <- c(0, length(dots) + 1)
    xtoplot <- seq(length(dots))
  }
  if(is.null(names(dots))) { # try to get names
    call <- as.list(match.call())[-1] # drop boxPlot
    call <- as.character(call)
    names(dots) <- call[seq(length(dots))]
  }
  ## Fix defaults for Box
  Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
                     censorstyle="", nobox=5, width="Auto", fill="none",
                     truncated=c(10,90))
  Box$type <- match.arg(Box$type, c("truncated", "simple",
  																	"tukey", "extended"))
  if(!is.na(Box$censorbox))
    warning(paste(Box$censorstyle, " boxplot not valid for these data, reset",
                  sep='')) # actually just ignored in call to this stats function
  ## Compute the stats and produce the boxplot
  if(dev.cur() == 1L)
    setGD("BoxPlot")
  statsret <- boxPlotStats(dots, Box, yaxis.log)
  ## What gets passed from statsret?
  retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
                          yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
                          xtitle, ytitle, caption, margin)
  invisible(retval)
}

#' @rdname boxPlot
#' @export
#' @method boxPlot list
boxPlot.list <- function(..., group=NULL, # data
												 Box=list(type="truncated", show.counts=TRUE, 
												 				 nobox=5, width="Auto", fill="none",
												 				 truncated=c(10,90)), # b&w controls
												 yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
												 ylabels="Auto", xlabels="Auto", xlabels.rotate=FALSE, # labels
												 xtitle="", ytitle="",  caption="", # axis titles and caption
												 margin=c(NA,NA,NA,NA)) { # margin control
	## Process data to plot, all must be class "list" and contain only numeric
	##  data. This would be the classic behavior for 
	if(!is.null(group))
		stop("Lists cannot be grouped by boxPlot")
	dots <- c(...)
	xrange <- c(0, length(dots) + 1)
	xtoplot <- seq(length(dots))
	## Fix defaults for Box
	Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
										 censorstyle="", nobox=5, width="Auto",  fill="none",
										 truncated=c(10,90))
	Box$type <- match.arg(Box$type, c("truncated", "simple",
																		"tukey", "extended"))
	if(!is.na(Box$censorbox))
		warning(paste(Box$censorstyle, " boxplot not valid for these data, reset",
									sep=''))
	## Compute the stats and produce the boxplot
	if(dev.cur() == 1L)
		setGD("BoxPlot")
	statsret <- boxPlotStats(dots, Box, yaxis.log)
	## What gets passed from statsret?
	retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
													yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
													xtitle, ytitle, caption, margin)
	invisible(retval)
}

#' @rdname boxPlot
#' @export
#' @method boxPlot data.frame
boxPlot.data.frame <- function(..., group=NULL, # data
															 Box=list(type="truncated", show.counts=TRUE,
															 				 nobox=5, width="Auto", fill="none",
															 				 truncated=c(10,90)), # b&w controls
															 yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
															 ylabels="Auto", xlabels="Auto",
															 xlabels.rotate=FALSE, # labels
															 xtitle="", ytitle="",  caption="", # axis titles and caption
															 margin=c(NA,NA,NA,NA)) { # margin control
	## Process data to plot, all must be class "data.frame" and only numeric
	##  data will be processed--all other classes of columns 
	if(!is.null(group))
		stop("Data frames cannot be grouped by boxPlot")
	dots <- c(...) # creates a list--drops data.frame attributes
	dots <- dots[sapply(dots, is.numeric)]
	xrange <- c(0, length(dots) + 1)
	xtoplot <- seq(length(dots))
	## Fix defaults for Box
	Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
										 censorstyle="", nobox=5, width="Auto",  fill="none",
										 truncated=c(10,90))
	Box$type <- match.arg(Box$type, c("truncated", "simple",
																		"tukey", "extended"))
	if(!is.na(Box$censorbox))
		warning(paste(Box$censorstyle, " boxplot not valid for these data, reset",
									sep=''))
	## Compute the stats and produce the boxplot
	if(dev.cur() == 1L)
		setGD("BoxPlot")
	statsret <- boxPlotStats(dots, Box, yaxis.log)
	## What gets passed from statsret?
	retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
													yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
													xtitle, ytitle, caption, margin)
	invisible(retval)
}
