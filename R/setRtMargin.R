# Set up Right Margin
#
# Coding History:
#    2014May21 DLLorenz Original coding.
#    2010Nov30 DLLorenz Modified for R
#    2011Oct24 DLLorenz Tweaks for package
#    2011Oct24          This version.
#

setRtMargin <- function(y, margin=c(NA,NA,NA,NA), right.labels=7, 
												right.log=FALSE, right.range=c(NA, NA)) {
  if(!is.na(margin[4L]))
  	warning("Original right margin no set to NA")
  if(any(is.na(right.range))) {
  	hard <- FALSE
  	right.range <- range(y, na.rm=TRUE)
  } else
  	hard <- TRUE
  if(right.log) {
  	RtMar <- logPretty(right.range, hard=hard, labels=labels)$margin
  } else 
  	RtMar <- linearPretty(right.range, hard=hard, labels=labels)$margin
  margin[4L] <- -RtMar
  invisible(margin)
}
