# Purpose: ellipse points,radially equispaced, given geometric par.s
#
# Coding history:
# Author: Martin Maechler, Date: 19 Mar 2002, 16:26
#    2010Mar21 DLLorenz Original coding, heavily modified from MM
#    2011Apr07 DLLorenz Begin modifications for R
#    2011Apr07          This version.
#

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
