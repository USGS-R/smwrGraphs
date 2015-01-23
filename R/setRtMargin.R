#' Set Right Margin
#' 
#' Set the right margin for graphs with secondary right axes. Used After
#' setting up the graphics environment, but before the call the the high-level
#' graphics function to allocate space for an additional right-axis label.
#' 
#' The values for \code{right.labels}, \code{right.log}, and \code{right.range}
#' should be set exactly as in the call to \code{addXY}.
#' 
#' @param y the secondary y-axis data to be plotted, missing values are
#' permitted and are ignored.
#' @param margin incomplete plot margin specification, generally computed by
#' \code{setGraph}.
#' @param right.labels set up right-axis labels; the approximate number of
#' labels.
#' @param right.log logical: log transform right axis?
#' @param right.range set right-axis range.
#' @return The updated margin; only the right margin value is changed.
#' @seealso \code{\link{setLayout}}, \code{\link{setGraph}},
#' \code{\link{addXY}}
#' @keywords dplot
#' @export setRtMargin
setRtMargin <- function(y, margin=c(NA,NA,NA,NA), right.labels=7, 
												right.log=FALSE, right.range=c(NA, NA)) {
	# Coding History:
	#    2014May21 DLLorenz Original coding.
	#    2010Nov30 DLLorenz Modified for R
	#    2011Oct24 DLLorenz Tweaks for package
	
	#
  if(!is.na(margin[4L]))
  	warning("Original right margin no set to NA")
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
