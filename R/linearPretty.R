# Compute nice looking labels for a linear axis
#
# Coding history:
#    2000Dec   Peter Shaw Original coding
#    2005Jul14 DLLorenz Original dated coding for library
#    2006Feb23 DLLorenz doc fix and modify for linear axis only
#    2008May02 DLLorenz Name change and modify labels to allow specific values
#    2008May03 DLLorenz Tweaks
#    2010Nov15 DLLorenz Modified for R
#    2011Oct24 DLLorenz Tweaks for package
#    2012Oct05 DLLorenz Nice integers and arg documentation
#    2014Feb04 DLLorenz Added extendrange function to default data.
#    2014May10 DLLorenz Added extend.range argument to suppress above
#

linearPretty <- function(x, hard=FALSE, labels="Auto", style="numeric",
                         extend.pct=0, extend.range=TRUE) {
  ## Arguments:
  ##  x - the numeric data
  ##  hard - force hard limits based on x?
  ##  labels - the number of labels, or the actual labels, or "auto" => 6
  ##   style - the style: numeric, with commas; scientific, scientific notation;
  ##           decimal, no comma
  ##  extend.pct - extend the axes by this percentage.
  ## function to get best fit to requested hard limits
  selbest <- function(xx, ticks, labels) {
    xrange <- range(xx)
    trange <- range(ticks)
    x1 <- as.vector(dist(xrange - trange))
    if(x1 < 1.e-6) return(ticks)
    ticks <- pretty(xx, n=2)
    trange <- range(ticks)
    x1 <- as.vector(dist(xrange - trange))
    if(x1 < 1.e-6) return(ticks)
    nint=3
    if(labels > 7) nint <- as.integer(labels / 2)
    ticks <- pretty(xx, n=nint)
    trange <- range(ticks)
    x1 <- as.vector(dist(xrange - trange))
    if(x1 < 1.e-6) return(ticks)
    nint <- as.integer(labels * 1.5)
    ticks <- pretty(xx, n=nint)
    trange <- range(ticks)
    x1 <- as.vector(dist(xrange - trange))
    if(x1 < 1.e-6) return(ticks)
    ## give up, use last ones and tack on range of xx
    ticks <- ticks[ticks > xrange[1L] & ticks < xrange[2L]]
    return(c(xrange[1L], ticks, xrange[2L]))
  } # end of sel best
  if(is.character(labels))
    labels = 6L
  xclean <- x[is.finite(x)]
  if (is.null(xclean))
    stop("all data are missing.")
  ## Extend range by a bit to avoid plotting on the axis. if requested
  if(extend.range)
  	xclean <- extendrange(xclean, f=0.01)
  if(length(labels) == 1L) {
    ticks <- pretty(xclean, n=labels-1L)
    ## if hard, then force limits of pretty to match limits of x
    if(hard) 
      ticks <- selbest(x, ticks, labels)
  }
  else # in effect forces hard
    ticks <- as.double(labels)
  
  ticxs <- ticks # set up intervals for labels
  ticks0 <- ticks #save for later
  ## examine the pretty intervals, to decide subdivisions.
  ## character tc will be "1", "2", or "5", depending on interval:
  ti<-diff(ticks)[1] #tick interval. (always positive).
  tc<-substring(format.default(ti,scientific=TRUE), 1L, 1L)
  
  ## choose subdivisor based on value of tc:
  tsub<-switch(tc, "1"=10, "2"=2, "5"=5)
  if (is.null(tsub))
    finegrid <- ticks # avoid crashing if weird axis limits specified
  else {
    dgrid <- (diff(ticks)[1L]) / tsub
    finegrid <- seq(ticks[1L], ticks[length(ticks)], dgrid)
  }
  style <- pmatch(style, c("numeric", "scientific"), nomatch=0L)
  if(style == 2L){ # style is scientific
    labs <- format(ticxs, scientific=TRUE)
    ## convert to expression
    labs <- format(labs) # seems to be needed to reset the call to scientific
    labs <- sapply(strsplit(labs, split='e', fixed=TRUE), function(x) {
      x <- as.numeric(x)
      as.expression(substitute(num %*% 10^exp, list(num=x[1], exp=x[2])))})
  }
  else if(style == 1L && max(ticxs) > 1000) { # numeric-- insert commas
    labs <- format(ticxs, big.mark=',', scientific=1)
    if(length(grep('e', labs, fixed=TRUE)) > 0L) { # used scientific notation
      labs <- format(labs) # seems to be needed to reset the call to scientific
      labs <- sapply(strsplit(labs, split='e', fixed=TRUE), function(x) {
        x <- as.numeric(x)
        as.expression(substitute(num %*% 10^exp, list(num=x[1L], exp=x[2L])))})
    }
    else
      labs <- strip.blanks(labs)
  }
  else # assume decimal, use no formatting
    labs <- format(ticxs)
### Note that if commas are inserted and the number is originally formatted
### like 1000.0, then the output is 1,000--the trailing .0 is dropped!
  ## return info
  yax <- list()
  yax$ticks <- ticks
  yax$finegrid <- finegrid
  yax$labels <- labs
  yax$labelpos <- ticxs
  yax$range <- range(ticks)
  if(extend.pct > 0) {
    if(hard)
      warning('no axis extension if hard is TRUE')
    else {
      midR <- sum(yax$range) / 2.
      yax$range <- (yax$range - midR)*(1 + extend.pct/50) + midR
    }
  }
  ## leaves enough for a two line title
  yax$margin <- max(strwidth(labs, units='inches', family='USGS'))/par('cin')[2L]+ 2.1 
  yax$style='at'
  return(yax)
}
