#' Biplot
#' 
#' Produce a biplot, which is a plot of two different types of data on the same
#' graph.
#' 
#' @param x any object which has a valid method for \code{biPlot}.
#' @param \dots additional arguments for other methods.
#' @return Information about the graph
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{biPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{biPlot.default}}, 
#' \code{\link{biPlot.princomp}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of biPlot:
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export
biPlot <- function(x, ...)
  UseMethod("biPlot")
