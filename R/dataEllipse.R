# compute the minimum covering ellipse for bivariate data
#
# Coding history:
#    2010Mar21 DLLorenz Original coding
#    2011Jun14 DLLorenz Begin conversion to R.
#    2010Jun14          This version.
#

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
