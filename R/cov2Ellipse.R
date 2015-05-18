#' Construct an Ellipse
#' 
#' Constructs an ellipse from a covariance matrix.
#' 
#' 
#' @param cov the 2-dimenstional covariance matrix, representing x and y.
#' @param center the means of x and y.
#' @param scale the size of the ellipse in units of standard deviation.
#' @param n the number of points in the returned data.
#' @return A list containing the x- and y-coordinates of the ellipse.
#' @seealso \code{\link{dataEllipse}}
#' @keywords dplot
#' @examples
#' # make a few points on a unit circle
#' TMP <- cov2Ellipse(matrix(c(1,0,0,1), ncol=2), c(0,0), n=5)
#' # Pretty print the data
#' lapply(TMP, zapsmall)
#' @export cov2Ellipse
cov2Ellipse <- function(cov, center, scale=1, n=151) {
  ## The arg cov must be a covariance matrix of N=2
  eig <- eigen(cov)
  B <- sqrt(min(eig$values)) # force known values
  A <- sqrt(max(eig$values))
  ## B <= A
  d2 <- (A-B)*(A+B)                   #= A^2 - B^2
  phi <- 2*pi*seq(0,1, len = n)
  sp <- sin(phi)
  cp <- cos(phi)
  r <- A*B / sqrt(B^2 + d2 * sp^2)*scale
  xy <- r * cbind(cp, sp)
  ## xy are the ellipse points for alpha = 0 and loc = (0,0)
  ##  theta <- asin(eig$vectors[1,1])
  ca <- eig$vectors[2,2]
  sa <- -eig$vectors[1,2]
  xy <- xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(center[1],n),
                                                    rep(center[2],n))
  return(list(x=xy[,1], y=xy[,2]))
}
