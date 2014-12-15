#' Pretty Axis
#' 
#' Construct information for making a "pretty" date axis.
#' 
#' 
#' @param x date and time data
#' @param major major tick interval, must be one of "hours," "days," "months,"
#' "years," or "Auto," which will make an intelligent choice from those. "Auto"
#' will also automatically change \code{tick.span}.
#' @param minor minor tick interval, must be one of "min," "hours," "days,"
#' "months," "years," or "Auto " which will make an intelligent choice from
#' those, possibly adjusted by a scaling factor. Can also be formatted as an
#' argument to \code{seq}, such as "15 mins" for 15-minute ticks.
#' @param tick.span span between major labels. For example, with "years"
#' option, tick.span=5 would generate labels like 1990, 1995, 2000 and so
#' forth.
#' @param style style of lables, must be one of "at," "between," or "Auto,"
#' which selects the "best" style. "At" places the labels at the ticks and
#' "between" places the labels between the major ticks.
#' @param label.abbr indicator of the degree of abbreviation for labels\cr
#' \tabular{lr}{ 0 \tab best guess based on number of intervals\cr 1 \tab
#' shortest (single letter month, for example)\cr 2 \tab standard abbreviation
#' (3-letter month name, for example)\cr 3 \tab full text\cr }
#' @return information about the axis labels.
#' @seealso \code{\link{timePlot}}
#' @keywords dplot
#' @export datePretty
datePretty <- function(x, major="Auto", minor="Auto", tick.span=1,
                       style =c("Auto", "at", "between"), label.abbr=0) {
	# Coding History:
	#    2006Feb17 DLLorenz Original coding.
	#    2007Apr06 DLLorenz Major overhaul
	#    2008May02 DLLorenz Name change
	#    2008May03 DLLorenz Tweaks
	#    2011Jan05 DLLorenz Begin modifications for R
	#    2013Jul02 DLLorenz More fine tuning
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  time.range <- range(x)
  ## Convert time.range to POSIXlt and fix the time zone info for consistency
  time.range <- as.POSIXlt(time.range)
  attr(time.range, "attr") <- "GMT"
  time.span <- difftime(time.range[2], time.range[1], units="days") 
  ## Returns class difftime, strip class
  time.span <- as.vector(time.span)
  if(major == "Auto") { # convert to best BETWEEN TICK option
    if(time.span > 365.*12) { # about 12 years
      major <- "years"
      splits <-     c(100,  50,  20, 10,  5, 2)
      splittest <- c(1200, 400, 160, 80, 30, 1)
      tick.span <- (time.span/365 + 1) > splittest
      if(any(tick.span))
        tick.span <- splits[tick.span][1]
      else
        tick.span <- 1000
    }
    else if(time.span > 364.*3) { # about 3 years
      major <- "years"
      tick.span <- 1
    }
    else if(time.span > 58) { # about 2 months
      major <- "months"
      tick.span <- 1
    }
    else if(time.span > 3) { # more than 3 days
      major <- "days"
      tick.span <- 1
    }
    else if(time.span > 1) { # between 1 and 3 days
      major <- "hours"
      tick.span <- 6
    }
    else { # must be pretty short
      major <- "hours"
      tick.span <- 1
    }
  } # end of major logic
  if(minor == "Auto") {
    if(major == "years") {
      if(tick.span > 1)
        minor <- "year"
      else
        minor <- "month"
    }
    else if(major == "months")
      minor <- "day"
    else if(major == "days")
      minor <- "hour"
    else # must be pretty short
      minor <- "15 mins"
  } # end of of minor logic
  ## determine label style logic: if major is "years" and tick.span > 1,
  ## then style is at tick. if major is "years", "months" or "days", then
  ## style is between ticks. For anything else, style is at tick.
  style=match.arg(style)
  if(style == "Auto") {
    if(major == "years" && tick.span > 1)
      style <- "at"
    else if(major == "days" && time.span > 27)
      style <- "at"
    else if(major %in% c("years", "months", "days")) {
      if(tick.span == 1)
        style <- "between"
      else
        style <- "at"
    } else
      style <- "at"
  } # end of style logic
  ## Extend the range to include the last time
  time.range$sec <- c(0,0) # Assume that we can ignore seconds
  if(major == "hours") {
    if(time.range$min[2] > 0)
      time.range[2] <- time.range[2] + as.difftime(1, units="hours")
    time.range$min <- c(0,0)
  }
  else if(major == "days") {
    time.range$min <- c(0,0) # Assume that we can ignore minutes 
    if(time.range$hour[2] > 0)
      time.range[2] <- time.range[2] + as.difftime(1, units="days")
    time.range$hour <- c(0,0)
  }
  else if(major == "months") {
    time.range$min <- c(0,0) # Assume that we can ignore minutes 
    if(time.range$mday[2] > 1 || time.range$hour[2] > 0) {
      ## Can't add 1 month, so need to do it manually
      if(time.range$mon[2] == 12) {
        time.range$mon[2] <- 1
        time.range$year[2] <- time.range$year[2] + 1
      }
      else
        time.range$mon[2] <- time.range$mon[2] + 1
    }
    time.range$hour <- c(0,0)
    time.range$mday <- c(1,1)
  }
  else { # Must be years
    time.range$min <- time.range$hour <- c(0,0) # Assume that we can ignore these 
    if(time.range$mday[2] > 1 || time.range$mon[2] > 1)
      time.range$year[2] <- time.range$year[2] + 1 # need to do manually
    time.range$mon <- c(1,1)
    time.range$mday <- c(1,1)
    ## Round to tick.spans
    years.spans <- (time.range$year %/% tick.span) * tick.span
    if(time.range$year[2] > years.spans[2])
      years.spans[2] <- years.spans[2] + tick.span
    time.range$year <- years.spans  
  }
  ticks <- as.POSIXlt(seq(from=time.range[1], to=time.range[2], 
                          by=paste(tick.span, major, sep=" ")))
  ## build labels from ticks
  N <- length(ticks)
  if(major == "years") {
    labels <- format(ticks, "%Y")
    label2 <- NULL
    label2pos <- NULL
    label2sep <- double(0)
  }
  else if(major == "months") {
    if(label.abbr == 0) {
      if(N > 15) # first guess for shortest
        label.abbr <- 1
      else if(N > 6) # first guess for standard abbr
        label.abbr <- 2
      else # use full length names
        label.abbr <- 3
    } # end label.abbr == 0
    labels <- format(ticks, "%B")
    if(label.abbr == 2)
      labels <- month.USGS[labels]
    else if(label.abbr == 1)
      labels <- substring(labels, 1, 1)
    ## generate year labels
    label2 <- format(ticks[-N], "%Y")
    ## Can use as.Date because these are integer days
    label2pos <- tapply(as.double(as.Date(ticks[-N])), label2, mean) + 15
    label2 <- names(label2pos)
    label2pos <- as.vector(label2pos)
    if(length(label2pos) > 1)
      label2sep <- as.double(as.Date(ticks[ticks$mon == 1]))
    else
      label2sep <- double(0)
  } # end of months
  else if(major == "days") {
    if(label.abbr == 0) {
      if(N > 15) # first guess for day number
        label.abbr <- 1
      else if(N > 6) # use month day & year as label2
        label.abbr <- 2
      else # use month day, year and no label2
        label.abbr <- 3
    } # end label.abbr == 0
    if(label.abbr == 1) {
      labels <- format(ticks, "%d")
      label2 <- paste(format(ticks[-N], "%B"),
                      format(ticks[-N], "%Y"), sep=", ")
      label2pos <- tapply(as.double(as.Date(ticks[-N])), label2, mean) + 0.5
      label2 <- names(label2pos)
      label2pos <- as.vector(label2pos)
      label2sep <- double(0)
    }
    else if(label.abbr == 2) {
      labels <- month.USGS[format(ticks, "%B")]
      labels <- paste(labels,	format(ticks, "%d"), sep=" ")
      label2 <- format(ticks[-N], "%Y")
      label2pos <- tapply(as.double(as.Date(ticks[-N])), label2, mean) + 0.5
      label2 <- names(label2pos)
      label2sep <- double(0)
    }
    else { # must be 3
      labels <- month.USGS[format(ticks, "%B")]
      labels <- paste(labels,	format(ticks, "%d, %Y"), sep=" ")
      label2 <- character(0)
      label2pos <- double(0)
      label2sep <- double(0)
    }
  }
  else { # major is hours
    if(style == "at") {
      labels <- sub(pattern="^00", replacement="24", x=format(ticks, "%H"))
      labels <- paste(labels, ":00", sep="")
    } else
      labels <- format(ticks, "%H")
    label2 <- format(ticks[-N], "%d")
    ## need to preserve the fractional parts of a day
    tickdate <- numericData(ticks)
    label2pos <- tapply(tickdate[-N], label2, mean) + 1./48.
    if(label.abbr == 0) {
      if(N > 24*5) # first guess for shortest
        label.abbr <- 1
      else if(N > 48) # first guess for standard abbr
        label.abbr <- 2
      else # use full length names
        label.abbr <- 3
    } # end label.abbr == 0
    ## convert back to POSIXt
    label2lab <- as.Date(label2pos, origin="1970-01-01")
    label2 <- switch(label.abbr,
    								 format(label2lab, format="%d/%m/%Y"),
                     {tmp <- month.USGS[format(label2lab, format="%B")]
                      paste(tmp, format(label2lab, format="%d, %Y"), sep=" ")},
    								 format(label2lab, format="%B %d, %Y"))
    label2sep <- as.double(as.Date(ticks[labels == "24H"]))
  } #end of label processing
## Convert to days and fractional days if nec.
  tick.range <- range(ticks)
  ticks <- numericData(ticks)
  ## labelposition at ticks by default
  labelpos <- ticks
  if(style == "between") {
    labels <- labels[-length(labels)]
    ## this puts the labels in the middle
    labelpos <- labelpos[-length(labelpos)] + diff(labelpos)/2
  }
  finegrid <- numericData(seq(tick.range[1], tick.range[2], by=minor))
  return(list(ticks=ticks, finegrid=finegrid, labels=as.vector(labels),
              labelpos=labelpos, range=range(ticks),
              label2=label2, label2pos=label2pos, label2sep=label2sep,
              style=style))
}
