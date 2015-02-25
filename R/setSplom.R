#' Scatter Plot Matrix
#' 
#' Set up a scatter plot matrix.
#' 
#' If an explanation is needed, then \code{explanation} is used to indicate
#' where the explanation is to be placed. The explanation can be placed either
#' to the right of the grid of graphs, at the bottom of the grid, or in one of
#' the grid cells.\cr To place an explanation to the right of the graphs,
#' \code{explantion} should be set to \code{list(right=ewid)}, where
#' \code{ewid} is the width of the explanation. In this case, the total of
#' \code{width} and \code{ewid} must be less that the total available for the
#' page.\cr To place an explanation at the bottom of the graphs,
#' \code{explantion} should be set to \code{list(bottom=ehei)}, where
#' \code{ehei} is the height of the explanation. In this case, the total pf
#' \code{height} and \code{ehei} must be less than the total available for the
#' page.\cr To place an explanation within a cell of the grid,
#' \code{explantion} should be set to \code{list(grid=enum)}, where \code{enum}
#' is the cell number in the grid. Cell numbers are sequential starting in the
#' upper left and increasing by column. In this case \code{num.graphs} must be
#' set to some number less than \code{num.cols} times \code{num.rows}.\cr
#' 
#' @param size the width and height of the entire graph area, exclusive of
#' explanation. If NULL, then use minimum of figure width and height.
#' @param num.variables the number of variables to plot.
#' @param show.all show the full grid? Otherwise only lower triangular graphs.
#' @param touching should individual graphs touch? Otherwise a small gap
#' separates individual graphs.
#' @param explanation a description of where to place the explanation if
#' needed. See \bold{Details}.
#' @param ymargin the left-margin for the plot area for the left column of
#' graphs.
#' @return a list like \code{setLayout} with three additional components:
#' \code{show.all}, \code{touching}, and \code{num.variables} from the call to
#' \code{setSplom}.
#' @seealso \code{\link{setLayout}}
#' @keywords dplot
#' @examples
#' \dontrun{
#' # See for examples of setSplom:
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @export setSplom
setSplom <- function(size=NULL, # Size of splom, must be square
                      num.variables, show.all=FALSE,
                      touching=TRUE, # Parameters of the splom
                      explanation=NULL, # Where to put explanation
                      ymargin=3.5) { # Margins for y- (and x-)axis
	# Coding History:
	#    2011Jul30 DLLorenz Original coding and begin tweaks
	#    2014Jun26 DLLorenz Converted to roxygen
	##
  if(is.null(size))
    size <- min(par('fin'))
  num.grid <- num.variables - (!show.all) # subtract 1 if lower, 0 if all
  shared <- as.double(!touching) # 0 for touching, 1 otherwise
  if(show.all) # allocate room at top/right for labels
    retval <- setLayout(width=size, height=size, num.cols=num.grid,
                        num.rows=num.grid, explanation=explanation,
                        shared.x=shared, shared.y=shared,yleft=ymargin,
                        yright=ymargin, xbottom=ymargin, xtop=ymargin)
  else
    retval <- setLayout(width=size, height=size, num.cols=num.grid,
                        num.rows=num.grid, explanation=explanation,
                        shared.x=shared, shared.y=shared,yleft=ymargin,
                        xbottom=ymargin)
  retval$show.all <- show.all
  retval$touching <- touching
  retval$num.variables <- num.variables
  invisible(retval)
}
