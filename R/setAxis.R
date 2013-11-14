# set up axis 
#
# Coding History:
#    2008Jun28 DLLorenz Original coding.
#    2009Dec02 DLLorenz Begin tweaks
#    2010Nov15 DLLorenz Modified for R
#    2011Jul29 DLLorenz Bug fix for ... 
#    2011Oct24 DLLorenz Tweaks for package
#    2012Jan10 DLLorenz bug fix for axis.range implies hard
#    2013Mar08 DLLorenz Added tweak to allow for date like formatting
#

setAxis <- function(data, axis.range, axis.log, axis.rev, axis.labels, ...) {
  ## arguments:
  ##   data - axis data
  ##   range - set axis range
  ##   axis.log, axis.rev - log or reverse data
  ##
  if(any(is.na(axis.range))) { # forced limits?
    drange <- range(data, na.rm=TRUE)
    hard <- FALSE
  }
  else {
    drange <- sort(axis.range)
    hard <- TRUE
  }
  if(axis.log) { # log transform?
    if('labels' %in% names(list(...)))
      dax <- logPretty(drange, hard=hard, ...)
    else
      dax <- logPretty(drange, hard=hard, labels=axis.labels, ...)
    data <- log10(data)
  }
  else {
    if('labels' %in% names(list(...)))
      dax <- linearPretty(drange, hard=hard, ...)
    else if(isDateLike(drange))
      dax <- datePretty(drange)
    else
      dax <- linearPretty(drange, hard=hard, labels=axis.labels, ...)
  }
  if(axis.rev) { # reverse sense of direction?
    data <- -data
    dax$ticks <- -dax$ticks
    dax$finegrid <- -dax$finegrid
    dax$labelpos <- -dax$labelpos
    dax$range <- rev(-dax$range) # need to maintain order
  }
  return(list(data=data, dax=dax))
}
