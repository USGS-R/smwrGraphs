# Compute nice looking labels for an arbitrary transfored axis
#
# Coding history:
#    2008Apr28 DLLorenz Original Coding inspired by trax, version 1:
#                                       Barry W. Brown
#                                       Department of Biomathematics, 
#                                            Box 237
#                                       University of Texas M. D.
#                                            Anderson Hospital
#                                       1515 Holcombe Blvd
#                                       Houston, TX 77030
#    2008May02 DLLorenz Name change and finegrid generation
#    2008May03 DLLorenz Tweaks
#    2010Nov29 DLLorenz Conversion to R
#    2011Oct24 DLLorenz Tweaks for package
#    2013Sep16 DLLorenz More tweaks
#

transPretty <- function(x, hard=FALSE, labels=11, style='none',
                        func=log, Ifunc=exp, ...) {
  ## Check to see that data are of a least length 2
  x <- x[is.finite(x)]
  if(length(x) < 2L)
    stop("x is not a vector of at least length 2")
  ## Check to see that labels is at least 3
  if(length(labels) == 1L) {
    if(labels < 3) labels <- 5
  }
  else { # set up for specific labels
    Xlabels <- as.double(labels)
    ## ... ?
  }
  ## transform x
  Xrange <- range(x)
  xTrange <- func(Xrange, ...)
  if(hard) {
    if(length(labels) == 1L)
      Trange <- xTrange
    else # take from labels
      Trange <- range(func(Xlabels, ...))
  }
  else {
    if(length(labels) == 1L) {
      xpretty <- range(pretty(x)) # need only upper and lower
      Trange <- func(xpretty, ...)
      if(!is.finite(Trange[1])) { # need to get a new low value
        xpretty[1L] <- 10^floor(min(log10(x), na.rm=TRUE))
        Trange <- func(xpretty, ...)
      }
   } else
     Trange <- range(func(Xlabels, ...))
  }
  ## Build ticks and labels
  if(length(labels) == 1L) { # build manually
    ticks <- Trange[1L] + seq(0,labels - 1)/(labels - 1) * diff(Trange)
    Xlabels <- Ifunc(ticks, ...)
    ## Make them look kind of pretty
    Ldiff <- floor(log10(diff(Xlabels)))
    if(Ldiff[1] < Ldiff[length(Ldiff)])
      Ldiff <- c(Ldiff[1], Ldiff)
    else
      Ldiff <- c(Ldiff, Ldiff[length(Ldiff)])
    Xlabels <- round(Xlabels, -c(Ldiff))
    ticks <- func(Xlabels, ...)
  } # end of build manually
  else
    ticks <- func(Xlabels, ...)
  ## Format labels, may need work
  style <- pmatch(style, c("numeric", "scientific"), nomatch=0)
  if(style == 0) # assume decimal, use no formatting
    labs <- as.character(Xlabels)
  else if(style == 1) # numeric-- insert commas
    labs <- format(Xlabels, big.mark=',', scientific=1)
  else { # style must be scientific
    labs <- format(Xlabels, scientific=TRUE)
    labs <- sub(pattern='e', replacement='x10^', labs)
  }
  labs <- strip.blanks(labs)
  ## No grid, use ticks
  yax <- list(ticks=ticks, finegrid=ticks, labels=labs, labelpos=ticks,
              range=Trange)
  yax$margin <- max(nchar(labs)) * .5 + 2.1
  yax$style='at'
  return(yax)
}
