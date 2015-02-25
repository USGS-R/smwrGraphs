#' Generate a Range of Colors
#' 
#' Generates a sequence of colors along a specified range.
#' 
#' @name smwr.colors
#' @rdname smwr.colors
#' @aliases smwr.colors blueRed.colors redBlue.colors coolWarm.colors 
#'warmCool.colors greenRed.colors redGreen.colors pastel.colors
#' @param n the number of colors to generate.
#' @param alpha a measure of the intensity of the generated colors
#' @return A sequence of character strings indicating the colors.
#' @note \code{blueRed.colors} generates a sequence from blue to red through
#' magenta. \code{redBlue.colors} generates a sequence from red to blue through
#' magenta. \code{coolWarm.colors} generates a sequence from blue to red
#' through green. \code{warmCool.colors} generates a sequence from red to blue
#' through green. \code{greenRed.colors} generates a sequence from green to red
#' through yellow. \code{redGreen.colors} generates a sequence from red to
#' green through yellow. \code{pastel.colors} genereates a sequence of
#' well-separated pastel colors useful for areas or bars.
#' @seealso \code{\link{rainbow}}, \code{\link{hcl}}
#' @keywords color
#' @examples
#' \dontrun{
#' # See for examples of warmCool.colors:
#' demo(topic="DurationHydrograph", package="smwrGraphs")
#' # All have similar usage
#' }
#' @export greenRed.colors
greenRed.colors <- function(n, alpha=1)
  rev(rainbow(n, start=0, end=1/3, alpha=alpha))
#
# Coding history:
#    2011Jun14 DLLorenz Original code.
#    2012May21 DLLorenz Added pastel.colors
#    2014Jun26 DLLorenz Converted to roxygen

#' @rdname smwr.colors
#' @export redGreen.colors
redGreen.colors <- function(n, alpha=1)
  rainbow(n, start=0, end=1/3, alpha=alpha)

#' @rdname smwr.colors
#' @export blueRed.colors
blueRed.colors <- function(n, alpha=1)
  rainbow(n, start=2/3, end=1, alpha=alpha)

#' @rdname smwr.colors
#' @export redBlue.colors
redBlue.colors <- function(n, alpha=1)
  rev(rainbow(n, start=2/3, end=1, alpha=alpha))

#' @rdname smwr.colors
#' @export warmCool.colors
warmCool.colors <- function(n, alpha=1) {
  ## Adjust linearity slightly to reduce preponderance of greens
  ## Rainbow color range from .05 to .55
  scl <- seq(0, 1, length.out=n)
  scl <- (12.5^scl - 1)/5.071068
  scl[scl > .5] <- 1 - rev(scl)[scl > .5] # Tricky way to symmetric scale
  scl <- 0.05 + 0.5*scl
  return(sapply(scl, function(x) rainbow(1, start=x, end=.99)))
}

#' @rdname smwr.colors
#' @export coolWarm.colors
coolWarm.colors <- function(n, alpha=1)
  rev(warmCool.colors(n))

#' @rdname smwr.colors
#' @export pastel.colors
pastel.colors <- function(n, alpha=1)
  hcl((seq(n) -1/(n + 4))*360/n , c=40, l=80, alpha=alpha) 
