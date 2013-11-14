# compute a parametric spline
#
# Coding history:
#    2003Sep23 DLLorenz Initial version
#    2009Mar19 DLLorenz Dated version
#    2011Dec30 DLLorenz USGSgraphs package
#    2012Mar13 DLLorenz Renamed to avoid conflict with survival
#    2012Sep10 DLLorenz Added stop for missing values.
#

paraSpline <- function(x, y, n) {
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
