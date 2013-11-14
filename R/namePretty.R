# Compute nice looking labels for a discrete value axis
#
# Coding History:
#    2011Jun22 DLLorenz Original coding.
#    2011Oct24 DLLorenz Tweaks for package
#    2011Oct24          This version.
#

namePretty <- function(x, orientation="table", order="none", label.abbr=FALSE,
                       offset=0.5) {
  ## Arguments:
  ## x (character or factor data) discrete data
  ## orientation (discrete value)
  ##    "table" - first in sequence at top (ends on right if x-axis)
  ##    "grid" - first in sequence at bottom 
  ## order (many choices)
  ##    "none" - accept order as is
  ##    "ascending" - sort in ascending alphabetical order
  ##    "descending" - sort in descending alphabetical order
  ##    named vector - sort by values (largest value at top if orientation is
  ##      "table")
  ##    character vector - put inot that order
  ## label.abbr (logical scalar)
  ##    F - no abbreviations
  ##    T - create abbreviations
  ##
  ## offset (numeric scalar) ammount to offset the range, generally 0.5 or 1
  ## Determine kind of order
  if(length(order) == 1)
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
  return(list(ticks=ticks, finegrid=ticks, labels=labels,
              labelpos=labelpos, range=range, style='at', margin=margin))
}
