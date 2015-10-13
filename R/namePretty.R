#' Pretty Axis
#' 
#' Construct information for making a nicely formatted axis for discrete data.
#'A support function for creating discrete axes. 
#' 
#' @param x the discrete data values.
#' @param orientation the orientation of the data in \code{x}.\cr \tabular{ll}{
#' "table" \tab first in sequence at top (ends on right if x-axis)\cr "grid"
#' \tab first in sequence at bottom \cr }
#' @param order the order of the data in \code{x}.\cr 
#' \tabular{ll}{ 
#' "none" \tab accept order as is\cr 
#' "ascending" \tab sort in ascending alphabetical order\cr
#' "descending" \tab sort in descending alphabetical order\cr 
#' named numeric vector \tab sort by values (largest value at top if orientation is
#' "table")\cr
#' character vector \tab specifies the sequence of names\cr }
#' @param label.abbr logical, if \code{TRUE}, then create abbreviations for \code{x},
#' otherwise use the full text of \code{x} for labels.
#' @param offset amount to offset the range, generally 0.5 or 1. The range of
#' the data is from 1 to the number of elements in \code{x}.
#' @param style character string indicating the placement of the ticks. If "at" (default),
#'then place ticks at the labels. If "between," then place ticks between the labels.
#' @return Information about the axis labels
#' @seealso \code{\link{dotPlot}}
#' @keywords dplot
#' @export namePretty
namePretty <- function(x, orientation="table", order="none", label.abbr=FALSE,
                       offset=0.5, style="at") {
	# Coding History:
	#    2011Jun22 DLLorenz Original coding.
	#    2011Oct24 DLLorenz Tweaks for package
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  ## offset (numeric scalar) ammount to offset the range, generally 0.5 or 1
  ## Determine kind of order
  if(length(order) == 1L)
    ckord <- match.arg(order, c("none", "ascending", "descending"))
  else
    ckord <- 0 # specified order
  ## Extract the unique values in x
  xc <- unique(as.character(x))
  if(inherits(x, 'factor') && ckord == 'none') # none
    xc <- levels(x[, drop=TRUE])
  else if(ckord == 'none') # Can;t let drop through
    xc <- xc
  else if(ckord == 'ascending') # ascending
    xc <- sort(xc)
  else if(ckord == 'descending') # descending
    xc <- rev(sort(xc))
  else { # must be specified--either sort by numeric or specified names
    if(is.numeric(order))
      ck1 <- names(order)[order(order)]
    else
      ck1 <- order
    if(length(ck1) != length(xc))
      stop("Incorrect number of values in order")
    if(any(!((ck1 %in% xc) | (xc %in% ck1))))
      stop("The sequence specified in order does not match all values in x")
    xc <- rev(ck1) # In keeping with the def above
  }
  ## set the orientation
  orientation <- match.arg(orientation, c("table", "grid"))
  if(orientation == "table") # table
    xc <- rev(xc)
  ticks <- seq(length(xc))
  fingrid <- ticks
  if(label.abbr)
    labels <- abbreviate(xc)
  else
    labels <- xc   
  labelpos <- ticks
  range <- c(1 - offset, length(xc) + offset)
  ## leaves enough for a two line title
  margin <- max(strwidth(labels, units='inches', family='USGS'))/par('cin')[2]+ 2.1 
  # Fix ticks if requested by style arg.
  if(style == "between") {
  	ticks <- (ticks[-1L] + ticks[-length(ticks)])/2
  } else if(style != "at") {
  	stop("Invalid style: ", style)
  }
  return(list(ticks=ticks, finegrid=ticks, labels=labels,
              labelpos=labelpos, range=range, style=style, margin=margin))
}
