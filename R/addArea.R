#' Add a Filled Polygon to Graph
#' 
#' Adds a filled polygon (area) to a graph
#' 
#' If \code{ybase} is NULL, then \code{x} and \code{y} should form a complete
#' polygon, which can be closed or open. Otherwise, \code{ybase} can be a single
#' value in which case the area between \code{ybase} and \code{y} is treated as
#' the area, or \code{ybase} can be a vector as long as \code{y} and the area
#' between is treated as the area to be shaded.
#' 
#' @param x the x-axis coordinates of the polygon. Missing values are not
#' permitted.
#' @param y the y-axis coordinates of the polygon. Missing values are not
#' permitted.
#' @param ybase the y-axis coordinates of the polygon. See \bold{Details}.
#' Missing values are not permitted.
#' @param Area parameters defining the characteristics of the area. See
#' \code{\link{areaPlot}} for details.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return The current plot information is returned invisibly.
#' @seealso \code{\link{areaPlot}}, \code{\link{addXY}}, \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- seq(1, 9, by=.5)
#' Y <- runif(17) + runif(17)
#' AA.pl <- xyPlot(X, Y, Plot=list(what="none"))
#' addArea(X, Y, ybase=0, current=AA.pl)
#' # For more details of addArea see
#' vignette(topic="GraphSetup", package="smwrGraphs")
#' demo(topic="DurationHydrograph", package="smwrGraphs")
#' }
#' @export addArea
addArea <- function(x, y, ybase=NULL, # data
                    Area=list(name="", color="gray",
                      outline="black"), # area controls
                    current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                      xaxis.log=FALSE)) { # current plot parameters
	# Coding History:
	#    2011Jun16 DLLorenz Original coding.
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Nov23 DLLorenz Added ybase option
	#    2013Aug30 DLLorenz Bug Fix for border = "none"
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  ## Process the data to plot
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- transData(as.double(x), current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  N <- length(x)
  if(!is.null(ybase)) { # Need to do something
  	ybase <- transData(ybase, current$yaxis.log, current$yaxis.rev,
  								 current$ytrans, current$ytarg)
    if(length(ybase) == 1L) { # a constant
      y <- c(ybase, y, ybase)
      x <- c(x[1L], x, x[N])
    }
    else { # ybase must be as long as y
      if(length(ybase) != N)
        stop("length of ybase must be one or match y")
      y <- c(y, rev(ybase))
      x <- c(x, rev(x))
    }
  }
  if(any(is.na(c(x, y))))
    stop("Missing value are not permitted in either x, y, or ybase")
  ## Set up plot information
  Area <- setDefaults(Area, name="", color="gray", outline="black")
  if(Area$outline == "none")
    Area$outline <- NA
  Plot <- setPlot(list(), name=Area$name, what="points", type="solid",
                  width="standard", symbol="none", filled=TRUE,
                  size=0.09, color="black", area.color=Area$color,
                  area.border=Area$outline) # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
  polygon(x, y, col=Area$color, border=Area$outline)
  current$x <- x
  current$y <- y
  current$explanation <- explan
  invisible(current)
}
