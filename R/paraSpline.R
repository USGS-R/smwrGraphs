#' Parametric Spline
#' 
#' Constructs a parametric interpolating spline for x and y data. The x data
#' are not required to be strictly increasing. Used as a support function.
#' 
#' 
#' @param x the x-coordinate data. Missing values are not permitted.
#' @param y the y-coordinate data. Missing values are not permitted.
#' @param n The number of points in the output parametric spline fit.
#' @return A list containing the components \code{x} and \code{y}, which are
#' the coordinates of the parametric spline.
#' @keywords dplot
#' @examples
#' paraSpline(c(1,2,3), c(0,1,0), n=5)
#' @export paraSpline
paraSpline <- function(x, y, n) {
	# Coding history:
	#    2003Sep23 DLLorenz Initial version
	#    2009Mar19 DLLorenz Dated version
	#    2011Dec30 DLLorenz USGSgraphs package
	#    2012Mar13 DLLorenz Renamed to avoid conflict with survival
	#    2012Sep10 DLLorenz Added stop for missing values.
	
  if(missing(y)) {
    if(is.list(x)) {
      y <- x$y
      x <- x$x
    } else
    stop("x must be a list or y must be supplied.")
  }
  if(length(x) != length(y))
    stop("lengths of x and y must agree.")
  s <- cumsum(c(0, sqrt(diff(x)^2+diff(y)^2)))
  if(any(is.na(s)))
    stop("Missing values are not permitted")
  if(missing(n))
    n <- 3*length(x)
  return(list(x=spline(s, x, n=n)$y, y=spline(s, y, n=n)$y))
}
