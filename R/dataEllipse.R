#' Construct an Ellipse
#' 
#' Construct an ellipse from x- and y-coordinate data.
#' 
#' 
#' @param x the x-coordinate data.
#' @param y the y-coordinate data.
#' @param percent a scale factor, adjusted to include \code{percent} of the
#' data.
#' @param smooth required for compatibiltiy, not used.
#' @return A list containing the x- and y-coordinates of the ellipse.
#' @seealso \code{\link{cov2Ellipse}}, \code{\link{hull}}
#' @keywords dplot
#' @export dataEllipse
dataEllipse <- function(x, y, percent=100, smooth=0) { 
  ## smooth required for compatibiltiy, not used
  ## Construct the data matrix and the ellispe info
  xy <- cbind(x,y)
  ell <- list(cov=var(xy), center=colMeans(xy), method='Covariance ellipse',
              X=xy)
  ## compute mahal distances for scale
  xy.dist <- sort(mahalanobis(xy, ell$center, cov=ell$cov))
  if(percent >= 99.99)
    ell$scale <- sqrt(max(xy.dist))
  else {
    sel <- as.integer(percent/100*length(x)) + 1
    ell$scale <- sqrt(xy.dist[sel])
  }
  return(cov2Ellipse(ell$cov, ell$center, ell$scale))
}
