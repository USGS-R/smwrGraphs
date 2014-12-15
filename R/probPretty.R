#' Pretty Axis
#' 
#' Construct information for making a nicely formatted probability axis.
#' 
#' 
#' @param x axis coordinates in range 0-1 or 0-100 allowed and assumed if
#' max(x) > 1 note that only min and max are needed, missing values allowed,
#' but ignored.
#' @param hard logical force min(x) and max(x) as axis limits, otherwise use
#' "nice" limits.
#' @param labels estimate of the number of labels desired, or label points if
#' vector note if vector, can be expressed as character strings, which are
#' converted to numeric and automatically scaled. Default is "Auto", which is 9
#' if minimum x is greater than .01 and 11 otherwise.
#' @param style can be either "probability" or "percent" indicates how the
#' labels are formatted.
#' @param exceedence \tabular{lr}{ TRUE \tab exceedence probs and additional
#' recurrence interval labels\cr FALSE \tab cumulative probabilities\cr }
#' @param priority \tabular{lr}{ "label" \tab "nice" labels given priority for
#' selection\cr "positions" \tab uniform separation given priority for
#' selection\cr }
#' @param distribution the name of the probability function, defaults to
#' normal.
#' @param \dots options for the distribution function.
#' @return Information about the axis labels.
#' @seealso \code{\link{probPlot}}
#' @keywords dplot
#' @export probPretty
probPretty <- function(x, hard=FALSE, labels='Auto', style='probability',
                       exceedence=TRUE, 
                       priority="label", distribution="normal", ...) {
  ## create ticks and labels for a probability axis
	# Coding History:
	#    2004Sep28 DLLorenz Original coding.
	#    2006Feb23 DLLorenz redate
	#    2006Mar06 DLLorenz modified to match output of other pretty functions
	#                       except finegrid
	#    2006Mar10 DLLorenz Added hard
	#    2006Aug14 DLLorenz Modifications for all distributions--start of coding
	#    2006Aug15 DLLorenz preliminary end of coding modifications
	#    2008May02 DLLorenz Name change
	#    2008May03 DLLorenz Start of tweaks
	#    2010Nov29 DLLorenz Conversion to R
	#    2011Jan08 DLLorenz Update comments
	#    2011Oct24 DLLorenz Tweaks for package
	#    2011Dec13 DLLorenz Fix for minx > .05 and labels > 9
	#    2013Jan29 DLLorenz Fix for fewer than 6 points to plot
	#    2014Jun27 DLLorenz Converted to roxygen
  ##
  ## get the distribution
  distribution=getDist.fcn(distribution, 'q')
  ## if labels are specified, then just make them
  if(length(labels) > 1) {
    Xlabels <- as.double(labels)
    labels <- as.character(labels)
    if(any(is.na(Xlabels)))
      stop("Invalid labels")
    if(max(Xlabels) > 1) # assume percentages
      Xlabels <- Xlabels / 100
    xticks <- distribution(Xlabels, ...)
    if(exceedence) {
      xticks <- rev(xticks)
      RI = as.character(round(1/Xlabels, 2))
      retval <- list(ticks=xticks, labels=labels, labelpos=xticks, range=range(xticks), RI=RI, style='at')
    }
    else
      retval <- list(ticks=xticks, labels=labels, labelpos=xticks,
                     range=range(xticks), style='at')
    return(retval)
  }
  ## Make best guess from range and label request.
  xmaxd <- max(x, na.rm=TRUE)
  xmind <- min(x, na.rm=TRUE)
  if(xmaxd > 1) { # assume expressed as percentage
    xmaxd <- xmaxd / 100
    xmind <- xmind / 100
  }
  if(xmaxd > 1 || xmind < 0)
    stop("probpretty: input data outside range (0-1)")
  ## last check for labels
  if(is.character(labels) || labels > 9)
    labels <- ifelse(xmind > .01, 9, 11)
  ## convert style
  probability <- pmatch(style, c('probability', 'percentage')) == 1
  ## allow limits to be 0 or 1 and adjust label limits to account for those limits
  txmn <- xmind
  if(xmind == 0.0) txmn <- 0.05
  txmx <- xmaxd
  if(xmaxd == 1.0) txmx <- 0.95
  xmin <- min(floor(log10(c(txmn, 1 - txmx))), -2) # find limits that bracket x
  if(is.null(hard)) {
    hard <- TRUE
    if(xmind != 0) xmind <- 10^xmin
    if(xmaxd != 1) xmaxd <- 1-xmind
  }
  ## start process of making labels, label and weight
  ## add intermediate powers of 10
  intlab <- 10^seq(-1, xmin, by=-1)
  lab <- intlab
  wtlab <- rep(1, length(intlab))
  ## add intermediate .5s
  lab <- c(lab, .5 * intlab)
  wtlab <- c(wtlab, rep(.8, length(intlab)))
  ## add intermediate .2s
  lab <- c(lab, .2 * intlab)
  wtlab <- c(wtlab, rep(.7, length(intlab)))
  ## add .2 to .4
  lab <- c(lab, .2, .3, .4)
  wtlab <- c(wtlab, .8, .7, .7)
  ## that's about all the labels that can be added!
  ## make the complete sequence with weights
  laborder <- order(lab)
  lab <- c(lab[laborder], .5, 1. - rev(lab[laborder]))
  wtlab <- c(wtlab[laborder], 1.5, rev(wtlab[laborder]))
  ## trim ticks and candidate labels to limits
  if(hard) {
    ToKeep <- lab > xmind+1.e-8 & lab < xmaxd-1.e-8 # adjust for numerical inacccuracy
    wtlab <- c(10, wtlab[ToKeep], 10)
    lab <- c(xmind, lab[ToKeep], xmaxd)
  }
  else {
    lowest <- which(lab > xmind + 1.e-8)[1] - 1 # need tiny offset
    highest <- rev(which(lab < xmaxd-1.e-8))[1] + 1
    wtlab <- wtlab[seq(from=lowest, to=highest)]
    wtlab[1] <- 10
    wtlab[length(wtlab)] <- 10
    lab <- lab[seq(from=lowest, to=highest)]
  }
  ## if limits are 0 or 1 remove ticks < 0.05 or > 0.95
  if(xmind == 0) {
    wtlab <- c(10, wtlab[lab >= 0.05])
    lab <- c(0, lab[lab >= 0.05])
  }
  if(xmaxd == 1) {
    wtlab <- c(wtlab[lab <= 0.95], 10)
    lab <- c(lab[lab <= 0.95], 1)
  }
  ## now make ticks and select labels
  xticks <- distribution(lab, ...)
  ## if only 2 labels, the pick ends
  if(labels == 2) {
    lab <- range(lab)
    lticks <- distribution(lab, ...)
  }
  else {
    priority <- pmatch(priority, c("labels", "positions"), nomatch=1)
    N <- length(lab)
    if(priority == 1 && N > labels) { # select labels by weighted label value
      lticks <- distribution(lab, ...)
      lrange <- diff(range(lticks))
      dists <- pmin(c(lrange, diff(lticks)), c(diff(lticks), lrange)) * wtlab
      ## remove the closest pairs
      NtoRemove <- (N - labels) %/% 2
      DistToTrim <- sort(dists)[NtoRemove] + 1.e-8 # adjust a bit to account for numerical errors
      lab <- lab[dists > DistToTrim]
      wtlab <- wtlab[dists > DistToTrim]
      ## select the widest pairs
      lticks <- distribution(lab, ...)
      lrange <- diff(range(lticks))
      dists <- pmin(c(lrange, diff(lticks)), c(diff(lticks), lrange)) * wtlab
      NtoKeep <- length(lticks) - labels + 1
      DistToKeep <- sort(dists)[NtoKeep] - 1.e-8 # same adjustment
      ## final selection
      lticks <- lticks[dists >= DistToKeep]
      lab <- lab[dists >= DistToKeep]
    } # end of label priority
    else if(N > labels) { # select label closest to uniform spacing
      lticks <- distribution(lab, ...)
      lrange <- range(lticks)
      pticks <- seq(from=lrange[1], to=lrange[2], length=labels)
      wtlab[1] <- wtlab[length(wtlab)] <- 1.5 # need to adjust down
      sel <- sapply(pticks, function(x, targ, wt) {z<- abs(x - targ)/wt; which(z == min(z))[1]},
                    targ=lticks, wt=wtlab)
      ## final selection
      lticks <- lticks[sel]
      lab <- lab[sel]
      
    } # end of posisiton priority
  } # end of labels > 2
  RI <- NULL
  if(exceedence) {
    lab <- 1 - lab
    RI <- as.character(round(1. / lab, -xmin))
  }
  if(probability)
    lab <- as.character(lab)
  else # want percentage
    lab <- as.character(lab * 100)
  return(list(ticks=xticks, labels=lab, labelpos=lticks, range=range(xticks),
              RI=RI, style='at'))
}
