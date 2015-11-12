#' Bar Graph
#' 
#' Create a bar chart by adding bars to an existing graph.
#' 
#' The \code{Bars} argument must be a tagged list with these components:
#' \describe{ 
#' \item{name}{a character vector describing each column of data; used in the 
#' explanation. If "Auto," then derive the name from the column names in \code{y}.}
#' \item{fill}{the name of the color to fill each bar. For multiple bars, can be
#' a vector of colors or the name of a color sequence generating function, such as
#' "pastel.colors."} 
#' \item{outline}{the name of the color to draw the outline or border for each 
#' bar. If "none," then no border is drawn.} 
#' \item{width}{the width of each bar in x-axis units. For discrete x-axis,
#' If \code{width} 1, then the bars form a continuous filled area.
#' The default is "Auto," which fills 2/3 of the distance. If 0, then draw vertical
#' lines rather than bars; the color of the line is based on \code{outline}.}
#' \item{orientation}{the orientation of the bars. Must be either "stack" or
#' "group." Can be abbreviated to a single letter.} } 
#' 
#' @param x the x-coordinate data. Missing values are permitted but result in
#' no bar.
#' @param y the heights of the bars or y-coordinate data. Missing values are
#'permitted but result in no bar. For stacked or grouped bars, \code{y} must be
#'a matrix, each row corresponding to the value in \code{x} and the columns
#'representing each bar. See \bold{Note}.
#' @param base extend the bars from this value.
#' @param Bars parameters defining the characteristics of the bars. See
#' \bold{Details}.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}. 
#' See \bold{Note}.
#' @return The current plot information is returned invisibly.
#' @note Use of \code{addBars} adds 1 step to creating bar charts, but adds
#'flexibility in axis formatted from existing high-level plotting functions such
#'as \code{xyPlot} or \code{timePlot}.
#' 
#'Bars are only valid for linear y-axes. Calling \code{addBars} when 
#'\code{yaxis.log} or \code{yaxis.rev} is \code{TRUE} or for any arbitrary 
#'transform of the y-axis will cause \code{addBars} to fail.
#' 
#'Datasets containing grouped data are often stacked with a column indicating the grouping.
#'There are several functions that will reformat stacked datasets. The \code{group2row}
#'function is very flexible in accepting many types of data to reformat rather than
#'only numeric data.
#' @seealso \code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{addXY}}
#Flip for production/manual
#'\code{\link[smwrBase]{group2row}}
#\code{group2row} (in smwrBase package)
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- seq(1, 9, by=1.0)
#' Y <- runif(9) + runif(9)
#' setGD()
#' AA.pl <- xyPlot(X, Y, Plot=list(what="none"), yaxis.range=c(0,2))
#' addBars(X, Y, base=0, current=AA.pl)
#' # For more details of addBars see
#' demo(topic="AnnualFlowBarChart", package="smwrGraphs")
#' }
#' @export addBars
addBars <- function(x, y, base=0,
                    Bars=list(name="Auto", fill="gray80", outline="black",
                      width="Auto", orientation="stack"),
                    current=list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE)) {
	# Coding history:
	#    2012Nov11 DLLorenz Original Coding
	#    2014Jun26 DLLorenz Converted to roxygen
	#    2015Jan07 DLLorenz Added width = 0 option to Bars
	#
	if(is.numeric(x) || isDateLike(x)) {
		x <- numericData(x)
	} else { # give the levels
		x <- numericData(x, lev=current$xax$labels)
	}
  y <- as.matrix(y)
  if(any(c(current$yaxis.log, current$yaxis.rev, !is.null(current$ytrans)))) {
  	stop("addBars requires a linear y-axis")
  }
  base <- rep(base, length.out=length(x))
  Bars <- setDefaults(Bars, name="Auto", fill="gray80", outline="black",
  										width="Auto", orientation="stack")
  Bars$orientation <- match.arg(Bars$orientation, c("stack", "group"))
  if(Bars$width == "Auto") {
  	xoff <- 1/3
  } else {
  	xoff <- Bars$width/2
  }
  if(ncol(y) == 1L) {
  	ybar <- y[,1L]
    if(xoff > 0) {
    	rect(x - xoff, base, x + xoff, ybar, col=Bars$fill,
    			 border=Bars$outline, lwd=lineWt("standard"))
    } else
    	segments(x, base, x, ybar, col=Bars$outline, lwd=lineWt("color"))
    if(Bars$name == "Auto") {
    	if(is.null(colnames(y)))
        name <- ""
      else
        name <- colnames(y)
    }
    else
      name <- Bars$name
    if(xoff > 0) {
    	Plot <- setPlot(list(), name=name, what='points', type='solid',
    									width='standard', symbol='none', filled=TRUE,
    									size=0.09, color='black', area.color=Bars$fill,
    									area.border=Bars$outline) # force defaults if not set
    } else
    	Plot <- setPlot(list(), name=name, what='vertical', type='solid',
    									width='standard', symbol='none', filled=TRUE,
    									size=0.09, color=Bars$outline)
    explan <- setExplan(Plot, old=current$explanation) # add info to set up explan
  } else {
  	if(xoff == 0) {
  		stop("Verical lines only valid for 1 column")
  	}
  	# Preliminary, common processing
  	explan <- current$explanation
  	if(Bars$name[1L] == "Auto") {
  		if(is.null(colnames(y)))
  			name <- paste0("Y", seq(ncol(y)))
  		else
  			name <- colnames(y)
  	}
  	else
  		name <- Bars$name
  	if(length(Bars$fill) == 1L) {
  		fill <- get(Bars$fill)(ncol(y))
  	} else
  		fill <- Bars$fill
  	if(Bars$orientation == "stack") {
  		# The code below accumulates the base to stack the data
  		for(i in seq(ncol(y))) {
  			ybar <- y[,i] + base
  			rect(x - xoff, base, x + xoff, ybar, col=fill[i],
  					 border=Bars$outline, lwd=lineWt("standard"))
  			Plot <- setPlot(list(), name=name[i], what='points', type='solid',
  											width='standard', symbol='none', filled=TRUE,
  											size=0.09, color='black', area.color=fill[i],
  											area.border=Bars$outline) # force defaults 
  			explan <- setExplan(Plot, old=explan)
  			base <- ybar
  		}
  	} else { # orientation must be "group"
  		xstart <- x - xoff
  		# Adjust width
  		xoff <- xoff/ncol(y)
  		xstart <- xstart + xoff
  		for(i in seq(ncol(y))) {
  			ybar <- y[,i]
  			rect(xstart - xoff + 2*xoff*(i - 1), base, 
  					 xstart + xoff+ 2*xoff*(i - 1), ybar, col=fill[i],
  					 border=Bars$outline, lwd=lineWt("standard"))
  			Plot <- setPlot(list(), name=name[i], what='points', type='solid',
  											width='standard', symbol='none', filled=TRUE,
  											size=0.09, color='black', area.color=fill[i],
  											area.border=Bars$outline) # force defaults 
  			explan <- setExplan(Plot, old=explan)
  		}
  	}
  }
  current$x <- x
  current$y <- y
  current$explanation <- explan
  invisible(current)
}

