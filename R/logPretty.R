# Compute nice looking labels for a log axis
#
# Coding history:
#    2000Dec   Peter Shaw Original Coding
#    2005Jul14 DLLorenz Original dated coding for library
#    2006Feb24 DLLorenz doc fix and generalize for any log-axis
#    2006Mar09 DLLorenz Fixed ticks at every minor location
#    2007Apr13 DLLorenz Fixed ticks and made automatic label
#    2008May02 DLLorenz Name change and modify labels to allow specific values
#    2008May03 DLLorenz Tweaks
#    2010Nov15 DLLorenz Modified for R
#    2011Jun17 DLLorenz Begin debug for minor ticks and other
#    2011Oct24 DLLorenz Tweaks for package
#    2014May20 DLLorenz Added dummy argument extend.range
#

logPretty <- function(x, hard=FALSE, labels="Auto", style='numeric',
                      extend.pct=0, extend.range=NA) {
  xclean <- x[is.finite(x)]
  xclean <- xclean[xclean>0]
  if (is.null(xclean))
    stop("(logPretty): input data all negative, null or NA")
  ticks <- pretty( xclean )
  ticxs <- ticks # set up intervals for labels
  ticks0 <- ticks #save for later
  
  ## examine the pretty intervals, to decide subdivisions.
  ## character tc will be "1", "2", or "5", depending on interval:
  ti<-diff(ticks)[1] #tick interval. (always positive).
  tc<-substring(format.default(ti,scientific=TRUE),1,1)
  
  ## choose subdivisor based on value of tc:
  xlogrange <- range(log10(xclean), na.rm=TRUE)
  tsub<-switch(tc, "1"=10, "2"=2, "5"=5)
  dgrid <- (diff(ticks)[1]) / tsub
  xlogspan <- diff(xlogrange)
  if(xlogspan > 1.5) {
    xlogrange[1] <- floor(xlogrange[1]) + 0.01
    xlogrange[2] <- ceiling(xlogrange[2]) - 0.01
    xlogspan <- diff(xlogrange)
  }
  if(length(labels) == 1) {
    ## if automatic label selection
    if(is.character(labels)) {# just assume auto, otherwise must be numeric
      if(xlogspan > 1.5) 
        labels <- floor(xlogspan + 1.1) # one for each cycle
      else
        labels <- floor(xlogspan * 9 + .99999) # label everything
    }
    ## Prune down the tick locations, then the grid.
    ## DLL: try to keep about labels number of labels
    labels.per.cycle <- as.integer(labels / xlogspan)
    tic<-1:9 # Complete set of tick locations per decade
    if(labels.per.cycle > 8) {
      ticx <- 1:9 # labels everywhere for now
    }
    else if(labels.per.cycle == 8) {
      ticx <- 1:8
    }
    else if(labels.per.cycle == 7) {
      ticx <- c(1:6,8)
    }
    else if(labels.per.cycle == 6) {
      ticx <- c(1:4,6,8)
    }
    else if(labels.per.cycle == 5) {
      ticx <- c(1,2,3,5,7)
    }
    else if(labels.per.cycle == 4) {
      ticx <- c(1,2,4,6)
    }
    else if(labels.per.cycle == 3) {
      ticx <- c(1,2,5)
    }
    else if(labels.per.cycle == 2) {
      ticx <- c(1,3) # Note this may require tweaking for certain stituations
    }
    else{
      ticx <- 1
    }
    grd <- c(seq(1, 4.9, .1), 
             seq(5, 9.8, .2)) # fine grid per decade
    x1 <- floor(xlogrange[1])-1
    x2 <- ceiling(xlogrange[2])+1
    ticks<-NULL
    ticxs<-NULL
    gridd<-NULL
    ## produce generous coverage of ticks, then prune to axis limits
    for (decade in x1:x2) {
      ticks <- c(ticks, (10^decade)*tic )  # tick locations
      ticxs <- c(ticxs, (10^decade)*ticx ) # label locations
      gridd <- c(gridd, (10^decade)*grd )  # grid locations
    }
    if (hard) {
      keep1 <- max(which(ticks<=min(xclean)))
      keep2 <- min(which(ticks>=max(xclean)))
      ticks <- ticks[keep1:keep2]
      keep1 <- max(which(ticxs<=min(xclean)))
      keep2 <- min(which(ticxs>=max(xclean)))
      ticxs <- ticxs[keep1:keep2]
    }
    else {
      keep1 <- max(1,which(ticks<10^xlogrange[1]))
      keep2 <- min(which(ticks>10^xlogrange[2]))
      ticks <- ticks[keep1:keep2] #prune tick locations
      keep1 <- max(1,which(ticxs<10^xlogrange[1]))
      keep2 <- min(which(ticxs>10^xlogrange[2]))
      ticxs <- ticxs[keep1:keep2] #prune labels
    }
    gridkeep1 <- min(which(gridd>=min(ticks)))
    gridkeep2 <- max(which(gridd<=max(ticks)))
    gridd <- gridd[gridkeep1:gridkeep2] #prune grid
    ## ticks, labels, and grid have now been pruned.
    
    ## Consider the pretty() locations;
    ## Prune down ticks to match pretty() ones: see what's left!
    tickkeep1 <- min(which(ticks>=min(ticks0)))
    tickkeep2 <- max(which(ticks<=max(ticks0)))
    if (tickkeep2>tickkeep1)
      tickQ <- ticks[tickkeep1:tickkeep2]
    else
      tickQ <- NULL
    ## fraction of y-axis used by data with this scheme
    dataspan <- diff(xlogrange) / diff(range(log10(ticks)))
    
    if ( ticks0[1]>0 && ( length(tickQ)<2 || dataspan<0.5 )) {
      ## ticks0 are useable & either pruning has been drastic,
      ##  or data does not fill enough of y-axis, so use 
      ##  the original pretty() ticks, ticks0.
      
      ticks <- ticks0 # straight from pretty()
      ticxs <- ticks0 # label each tick mark.
      
      gridd <- seq(ticks0[1], ticks0[length(ticks0)], dgrid)
    }
  } # end of automatic generation of ticks and labels
  else {
    ticks <- labels
    grd <- c(seq(1, 4.9, .1), 
             seq(5, 9.8, .2)) # fine grid per decade
    x1 <- floor(xlogrange[1])-1
    x2 <- ceiling(xlogrange[2])+1
    gridd<-NULL
    ## produce generous coveragefor fine grid
    for (decade in x1:x2) {
      gridd <- c(gridd, (10^decade)*grd )  # grid locations
    }
    gridkeep1 <- min(which(gridd>=min(ticks)))
    gridkeep2 <- max(which(gridd<=max(ticks)))
    gridd <- gridd[gridkeep1:gridkeep2] #prune grid
    ticxs <- labels
  }
  yax <- list()
  yax$ticks <- log10(ticks)
  yax$finegrid <- log10(gridd)
  yax$labelpos <- log10(ticxs)
  ## set label style
  style <- pmatch(style, c("numeric", "scientific"), nomatch=0)
  if(style == 0) # assume decimal, use no formatting
    labs <- as.character(ticxs)
  else if(style == 1) { # numeric-- insert commas
    labs <- format(ticxs, big.mark=',', scientific=1)
    if(length(grep('e', labs, fixed=TRUE)) > 0) { # used scientific notation
      labs <- format(labs) # seems to be needed to reset the call to scientific
      labs <- sapply(strsplit(labs, split='e', fixed=TRUE), function(x) {
        x <- as.numeric(x)
        as.expression(substitute(num %*% 10^exp, list(num=x[1], exp=x[2])))})
    }
    else
      labs <- strip.blanks(labs)
  }
  else { # style must be scientific
    labs <- format(ticxs, scientific=TRUE)
    ## convert to expression
    labs <- format(labs) # seems to be needed to reset the call to scientific
    labs <- sapply(strsplit(labs, split='e', fixed=TRUE), function(x) {
      x <- as.numeric(x)
      as.expression(substitute(num %*% 10^exp, list(num=x[1], exp=x[2])))})
  }
  yax$labels <- labs
  yax$range <- range(c(yax$ticks, yax$labelpos))
  if(extend.pct > 0) {
    if(hard)
      warning('no axis extension if hard is TRUE')
    else {
      midR <- sum(yax$range) / 2.
      yax$range <- (yax$range - midR)*(1 + extend.pct/50) + midR
    }
  }
  ## leaves enough for a two line title
  yax$margin <- max(strwidth(labs, units='inches', family='USGS'))/par('cin')[2]+ 2.1 
  yax$style='at'
  return(yax)
}
