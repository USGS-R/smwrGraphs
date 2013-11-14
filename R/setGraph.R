# Set up graph fop a multi graph page
#
# Coding History:
#    2008Jun27 DLLorenz Original coding.
#    2010Nov30 DLLorenz Modified for R
#    2011Oct24 DLLorenz Tweaks for package
#    2011Oct24          This version.
#

setGraph <- function(graphNum, layout, noTicks=NULL) {
  ## this is easy (maybe)
  par(fig=layout[[graphNum]]$fig)
  margin <- layout[[graphNum]]$margin
  ## this code protects against NAs
  if(!is.null(noTicks))
    margin[noTicks] <- ifelse(margin[noTicks] == 0, -101, -margin[noTicks])
  par(new=TRUE)
  invisible(margin)
}
