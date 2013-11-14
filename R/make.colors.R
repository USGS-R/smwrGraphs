# some color range generators
#
# Coding history:
#    2011Jun14 DLLorenz Original code.
#    2012May21 DLLorenz Added pastel.colors
#    2012May21          This version.
#

greenRed.colors <- function(n, alpha=1)
  rev(rainbow(n, start=0, end=1/3, alpha=alpha))

redGreen.colors <- function(n, alpha=1)
  rainbow(n, start=0, end=1/3, alpha=alpha)

blueRed.colors <- function(n, alpha=1)
  rainbow(n, start=2/3, end=1, alpha=alpha)

redBlue.colors <- function(n, alpha=1)
  rev(rainbow(n, start=2/3, end=1, alpha=alpha))

warmCool.colors <- function(n, alpha=1) {
  ## Adjust linearity slightly to reduce preponderance of greens
  ## Rainbow color range from .05 to .55
  scl <- seq(0, 1, length.out=n)
  scl <- (12.5^scl - 1)/5.071068
  scl[scl > .5] <- 1 - rev(scl)[scl > .5] # Tricky way to symmetric scale
  scl <- 0.05 + 0.5*scl
  return(sapply(scl, function(x) rainbow(1, start=x, end=.99)))
}

coolWarm.colors <- function(n, alpha=1)
  rev(warmCool.colors(n))

## pastel.colors provides well-separated colros for areas and bars.
pastel.colors <- function(n, alpha=1)
  hcl((seq(n) -1/(n + 4))*360/n , c=40, l=80, alpha=alpha) 
