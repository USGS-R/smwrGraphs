#' Construct a Hull
#' 
#' Construct an enclosing hull from x- and y-coordinate data.
#' 
#' 
#' @param x the x-coordinate data. Missing values are permitted, but ignored.
#' @param y the y-coordinate data. Missing values are permitted, but ignored.
#' @param percent the minimum percent to enclose.
#' @param smooth smooth the bounding hull?
#' @return A list containing the x- and y-coordinates of the hull.
#' @seealso \code{\link{dataEllipse}}
#' @keywords dplot
#' @examples
#' \dontrun{
#' # See for examples of hull:
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' }
#' @export hull
hull <- function(x, y, percent=100, smooth=FALSE) {
	# Coding History:
	#    2010Mar20 DLLorenz Original coding
	#    2011Jun14 DLLorenz Begin conversion to R.
	#    2011Oct24 DLLorenz Tweaks for package
	#    2014Jun26 DLLorenz Converted to roxygen
  ## 
  ## The smoothing function
  pspline <- function(x, y) {
    s <- cumsum(c(0, sqrt(diff(x)^2+diff(y)^2)))
    n <- 151
    return(list(x=spline(s, x, method='periodic', n=n)$y,
                y=spline(s, y, method='periodic', n=n)$y))
  }
  ## Code starts here
  N <- length(x)
  pts <- chull(x, y)
  Nhull <- length(pts)
  ## Catch minimum percent
  if(percent < (3 / N) * 100) {
    percent <- (3.01 / N) * 100
    warning('percent of data too small, reset to minimum for data')
  }
  if(percent > (N-1)/N * 100) {
    pts <- c(pts, pts[1]) # Close the loop
    x <- x[pts]
    y <- y[pts]
  }
  else {
    Nlast <- N
    ## Peel away the hull so that at least percent % of the data are
    ## enclosed in the hull
    while(percent < (Nlast - Nhull) / N * 100) {
      Nlast <- Nlast - Nhull
      x <- x[-pts]
      y <- y[-pts]
      pts <- chull(x, y)
      Nhull <- length(pts)
    }
    ## How many to retain?
    Nret <- as.integer(N / 100 * percent + 1)
    NtoRemove <- Nlast - Nret
    if(NtoRemove == 0) { # Easy use these data
      pts <- c(pts, pts[1])
      x <- x[pts]
      y <- y[pts]
    }
    else {
      ## OK, now eliminate the fartest points out to make the correct percent
      Out <- cbind(x=x, y=y)
      In <- cbind(x=x[-pts], y=y[-pts])
      ptsIn <- chull(In)
      dists <- outer(pts, ptsIn, function(x, y, xd, yd) 
                     sqrt((xd[x, 1] - yd[y,1])^2 + (xd[x, 2] - yd[y,2])^2),
                     xd=Out, yd=In)
      whichMin <- apply(dists, 1, function(x) which(min(x) == x))
      distMin <-  apply(dists, 1, min)
      rankMin <- rank(-distMin) # This identifies the farthest away
      Drop <- pts[rankMin <= NtoRemove]
      x <- x[-Drop]
      y <- y[-Drop]
      pts <- chull(x, y)
      pts <- c(pts, pts[1])
      x <- x[pts]
      y <- y[pts]
    }
  }
  if(smooth)
    return(pspline(x, y))
  return(list(x=x, y=y))
}
