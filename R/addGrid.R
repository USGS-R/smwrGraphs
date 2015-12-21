#' Grid Lines
#' 
#' Adds grid lines to a graph.
#' 
#' Information about grid lines is contained in the information returned from
#' high-level plotting functions in the smwrGraphs package. 
#' 
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}. See
#' \bold{Details}.
#' @param Xgrid parameters defining the characteristics of the x-axis grid
#' lines. The components refer to the color to draw the \code{grid} (at ticks)
#' or \code{finegrid} (between ticks).
#' @param Ygrid  parameters defining the characteristics of the y-axis grid 
#' lines. The components refer to the color to draw the \code{grid} (at ticks)
#' or \code{finerid} (between ticks).
#' @return NULL is returned invisibly.
#' @note The function \code{addGrid} should be used after
#' setting up a graph with one of the main plotting functions in the smwrGraphs
#' package and setting the \code{what} component in the \code{Plot} argument to
#' "none." The graph can be completed by using \code{addXY}.
#' 
#' @seealso \code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{addXY}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' setGD()
#' AA.pl <- xyPlot(X, Y, Plot=list(what="none"))
#' # Grid first, then data to avoid over plotting
#' addGrid(AA.pl)
#' addXY(X, Y, Plot=list(what="points"))
#' # For more details of addGrid see
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' }
#' @export addGrid
addGrid <- function(current, Xgrid=list(grid="gray50", finegrid="none"),
                    Ygrid=list(grid="gray50", finegrid="none")) {
	# Coding history:
	#    2012Nov11 DLLorenz Original Coding
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  Xgrid <- setDefaults(Xgrid, grid="gray50", finegrid="none")
  Ygrid <- setDefaults(Ygrid, grid="gray50", finegrid="none")
  if(!is.null(current$xax)) {
    Not <- current$xax$range
    if(Xgrid$grid != "none") {
      Grd <- current$xax$ticks
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(v=Grd, col=Xgrid$grid, lwd=frameWt())
    }
    if(Xgrid$finegrid != "none") {
      Grd <- current$xax$finegrid
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(v=Grd, col=Xgrid$finegrid, lwd=frameWt())
    }
  }
  if(!is.null(current$yax)) {
    Not <- current$yax$range
    if(Ygrid$grid != "none") {
      Grd <- current$yax$ticks
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(h=Grd, col=Ygrid$grid, lwd=frameWt())
    }
    if(Ygrid$finegrid != "none") {
      Grd <- current$yax$finegrid
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(h=Grd, col=Ygrid$finegrid, lwd=frameWt())
    }
  }
  invisible()
}
