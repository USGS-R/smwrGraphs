#' Pretty Axis
#' 
#' Construct information for making a a nicely formatted date axis.
#' 
#' 
#' @param x time difference data
#' @param labels either "Auto," which lets the function decide how many labels,
#' the approximate number of labels, or the actual labels to use.
#' @return information about the axis labels.
#' @seealso \code{\link{timePlot}}
#' @keywords dplot
#' @export timePretty
timePretty <- function(x, labels="Auto") {
	#
	# Coding History:
	#    2013Jul02 DLLorenz Original coding.
	#    2014Jun27 DLLorenz Converted to roxygen
  ##
  time.range <- as.numeric(range(x))
  delt <- attr(x, "units")
  if(delt %in% c("secs", "mins")) {
    if(time.range[2L] <= 10) {
      time.range[1L] <- floor(time.range[1L])
      time.range[2L] <- ceiling(time.range[2L])
    } else if(time.range[2L] <= 15) {
      time.range[1L] <- (time.range[1L] %/% 15) * 15
      time.range[2L] <- 15
    } else if(time.range[2L] <= 300) {
      time.range[1L] <- (time.range[1L] %/% 30) * 30
      time.range[2L] <- ((time.range[2L] - 0.0001) %/% 30 + 1) * 30
    } else { # Else switch to a different unit!
      time.range[1L] <- (time.range[1L] %/% 60) * 60
      time.range[2L] <- ((time.range[2L] - 0.0001) %/% 60 + 1) * 60
    }
    hard <- TRUE
  } else if(delt == "hours") {
      if(time.range[2L] <= 6) {
      time.range[1L] <- floor(time.range[1L])
      time.range[2L] <- ceiling(time.range[2L])
    } else if(time.range[2L] <= 9) {
      time.range[1L] <- (time.range[1L] %/% 9) * 9
      time.range[2L] <- 9
    } else if(time.range[2L] <= 48) {
      time.range[1L] <- (time.range[1L] %/% 6) * 6
      time.range[2L] <- ((time.range[2L] - 0.0001) %/% 6 + 1) * 6
      if(is.character(labels)) # Must be "Auto"
        labels <- seq(time.range[1L], time.range[2L], by = 6)
    } else { # Else switch to a different unit!
      time.range[1L] <- (time.range[1L] %/% 24) * 24
      time.range[2L] <- ((time.range[2L] - 0.0001) %/% 24 + 1) * 24
      if(is.character(labels)) {
        if(diff(time.range) > 4*24)
          labels <- seq(time.range[1L], time.range[2L], by = 24)
        else
          labels <- seq(time.range[1L], time.range[2L], by = 12)
      }
    }
    hard <- TRUE
  } else
    hard <- FALSE # Let 'em float
  return(linearPretty(time.range, hard=hard, labels=labels, extend.range = FALSE))
}
