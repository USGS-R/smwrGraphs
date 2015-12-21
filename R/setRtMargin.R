#' Set Secondary Margin
#' 
#' Sets the right or top margin for graphs with secondary axes. Used After
#' setting up the graphics environment, but before the call to the high-level
#' graphics function to allocate space for an additional label and title.
#' 
#' @details The values for \code{right.labels}, \code{right.log}, and \code{right.range}
#' should be set exactly as in the call to \code{addXY}.
#' 
#' The \code{margin} is a numeric vector of length 4 specifying the bottom, left, top, 
#'and right margins around the plot, as decriibed in by the \code{mar} option in 
#'\code{\link{par}}. The function \code{setTopMargin} only changes the third value and
#'the function \code{setRtMargin} only changes the fourth value. The value for 
#'\code{margin} is typically the output from \code{setGraph} or the defaults for 
#'these functions.
#'
#' 
#' @param y the secondary y-axis data to be plotted, missing values are
#' permitted and are ignored.
#' @param margin incomplete plot margin specification, generally computed by
#' \code{setGraph}.
#' @param right.labels set up right-axis labels; the approximate number of
#' labels.
#' @param right.log logical, if \code{TRUE}, then log transform right axis.
#' @param right.range set right-axis range.
#' @return The updated margin; only the right margin value is changed.
#' @seealso \code{\link{setLayout}}, \code{\link{setGraph}},
#' \code{\link{addXY}}
#' @keywords dplot
#' @examples
#' \dontrun{
#' # See for examples of setRtMargin:
#' demo(topic="RightAxisExample", package="smwrGraphs")
#' # See for examples of setTopMargin:
#' demo(topic="TopAxisExample", package="smwrGraphs")
#' }
#' @export setRtMargin
setRtMargin <- function(y, margin=c(NA,NA,NA,NA), right.labels=7, 
												right.log=FALSE, right.range=c(NA, NA)) {
	# Coding History:
	#    2014May21 DLLorenz Original coding.
	#    2010Nov30 DLLorenz Modified for R
	#    2011Oct24 DLLorenz Tweaks for package
	#    2015jan27 DLLorenz & LDeCicco  Bug fix and add Top
	#
  if(!is.na(margin[4L]))
  	warning("Original right margin not set to NA")
  if(any(is.na(right.range))) {
  	hard <- FALSE
  	right.range <- range(y, na.rm=TRUE)
  } else
  	hard <- TRUE
  if(right.log) {
  	RtMar <- logPretty(right.range, hard=hard, labels=right.labels)$margin
  } else 
  	RtMar <- linearPretty(right.range, hard=hard, labels=right.labels)$margin
  margin[4L] <- -RtMar
  invisible(margin)
}

#' @rdname setRtMargin
#' @export
setTopMargin <- function(margin=c(NA,NA,NA,NA)) {
	if(!is.na(margin[3L]))
		warning("Original top margin not set to NA")
	margin[3L] <- -2.2
	invisible(margin)
}
