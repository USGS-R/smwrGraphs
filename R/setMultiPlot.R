#' Plot Parameters
#' 
#' Sets plot control list for individuals in data (support function).
#' 
#' Each of the arguments from \code{name} through \code{color} must have one
#' entry for each observation. If a single value is given, then it is
#' replicated for each observation. In general, it is convenient to set up a
#' data frame with columns for group names with common values for the line or
#' symbol. It is required that each group have common values for the line or
#' symbol.
#' 
#' @param current the plot parameters specified in the call to the high-level
#' graphing function.
#' @param Nobs the number of observations.
#' @param name the name associated with the observation. See \bold{Details}.
#' @param what what kind of plot. Must be one of "points," symbols only;
#' "lines," line segments connecting points only; "both," line segments
#' connecting isolated symbols; "overlaid," line segments connecting points
#' with symbols; "stairstep," stairstep line segments; or "vertical," vertical
#' lines from the y-axis origin to the y value at each x value.
#' @param type the type of line, if drawn. Must be one of "solid," "dashed,"
#' "dotted."
#' @param width the width of the line, if drawn. Must be one of "standard,"
#' resulting in a line width of about 0.7 points; "color," resulting in a line
#' width of about 0.8 points; "bold," resulting in a line width of about 1
#' point; or "hairline" resulting in a line width of about 0.5 points. Note
#' these values are doubled if the \code{font} argument to \code{setPage} is
#' "PPT."
#' @param symbol type symbol, if drawn. Must be one of "circle;" "uptri,"
#' upward pointing triangle; "plus;" "x;" "diamond;" "downtri," downward
#' pointing triangle; "square;" or "dot."
#' @param filled logical, if \code{TRUE}, then fill the symbol. Valid only for
#' \code{symbol} equal to "circle," "uptri," "diamond," "downtri," or "square."
#' @param size the size of the symol in inches, if drawn.
#' @param color the color of the plotted values. Can be a named color, such as
#' "black" or "gray50" or an RGB color like "#4056FF."
#' @param order specify the order of the symbols in the explanation. Can be "as
#' is'--do nothing to order in explanation, "sort" or "increasing"---put into
#' sorted order, "decreasing"--- put in reverse order; or a vector theat
#' specifies the exact order.
#' @return A list having two components:\cr \item{current}{a list like
#' \code{current} with the defaults set} \item{Explan}{a list for creating an
#' explanation}
#' @seealso 
#Flip for production/manual
#'\code{\link{colors}} for a list of color names,
#\code{colors} (in grDevices package) for a list of color names,
#' \code{\link{setExplan}}, for details about the list required for an 
#'explanation.
#' @keywords dplot
#' @export setMultiPlot
setMultiPlot <- function(current, Nobs=1, name="", what='points', type='solid',
                         width='standard', symbol='circle', filled=TRUE,
                         size=0.09, color='black', order='as is') {
	# Coding history:
	#    2008Oct27 DLLorenz Original Code and begin of tweaks
	#    2011Jan12 DLLorenz Conversion to R
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Jun14 DLLorenz Added option to set the order in the explanation
	#    2011Jun16 DLLorenz Added empty info for areas
	#    2011Oct24 DLLorenz Tweaks for package
	#    2014Jun26 DLLorenz Converted to roxygen.
	#
  if(is.null(current$name))
    current$name <- name
  current$name <- rep(as.character(current$name), length.out=Nobs)
  
  if(is.null(current$what))
    current$what <- what
  current$what <- rep(current$what, length.out=Nobs)
  current$what <- pmatch(current$what, c("points", "lines", "both", "overlaid",
                                         "stairstep", "vertical"), nomatch=1,
                         duplicates.ok=TRUE)
  current$what <- c("p", "l", "b", "o", "s", "h")[current$what]
  ## what (type argument) stays as what
  
  if(is.null(current$type))
    current$type <- type
  current$type <- rep(current$type, length.out=Nobs)
  current$type <- pmatch(current$type, c("solid", "dashed", "dotted"), nomatch=1,
                         duplicates.ok=TRUE)
  current$type <- c("solid", "dashed", "dotted")[current$type]
  current$lty <- rep(1, Nobs) ## required for lty argument
  
  if(is.null(current$width))
    current$width <- width
  current$width <- rep(current$width, length.out=Nobs)
  current$width <- pmatch(current$width, c("standard", "color", "bold", "hairline"),
                          nomatch=1, duplicates.ok=TRUE)
  current$width <- c("standard", "color", "bold", "hairline")[current$width]
  current$lwd <- rep(1, Nobs) ## required for lwd argument
  
  if(is.null(current$symbol))
    current$symbol <- symbol
  current$symbol <- rep(current$symbol, length.out=Nobs)
  current$symbol <- pmatch(current$symbol, c("circle", "uptri", "plus", "x",
                                             "diamond", "downtri", "square", "dot",
                                             "+", "none"), nomatch=1,
                           duplicates.ok=TRUE)
  current$symbol <- c("circle", "uptri", "plus", "x", "diamond", "downtri", 
                      "square", "dot", "plus", "none")[current$symbol]
  current$pch <- rep(1, Nobs) ## required for pch argument
  
  if(is.null(current$filled))
    current$filled <- filled
  current$filled <- rep(current$filled, length.out=Nobs)
  current$filled <- ifelse(current$filled & current$symbol %in%
                           c("plus", "x", "dot", "none"), 
                           FALSE, current$filled) # fix it
  
  if(is.null(current$size))
    current$size <- size
  current$size <- rep(current$size, length.out=Nobs)
  current$csi <- rep(1.5, Nobs) ## required for csi argument
  
  if(is.null(current$color))
    current$color <- color
  current$color <- rep(current$color, length.out=Nobs)
  current$col <- current$color # Not sure why col is needed
  ## Add the empty area info
  current$area.color <- rep(NA, length.out=Nobs)
  current$area.border <- rep(NA, length.out=Nobs)
  ## OK now, we've got everything filled out, make an explanation and code symbols
  compressed <- unique(as.data.frame(current,stringsAsFactors=FALSE))
  uniqNames <- unique(current$name)
  if(length(uniqNames) != nrow(compressed))
    stop("Multiple lines/symbols must have associated names in Plot argument")
  ## Sort if necessary
  if(order[1] != 'as is') { # need to do somthing
    if(order[1] %in% c('sort', 'increasing'))
      Seq <- order(uniqNames)
    else if(order[1] == 'decreasing')
      Seq <- length(uniqNames) - order(uniqNames) + 1
    else # order must specify the order
      Seq <- match(order, uniqNames)
    ## Put into order
    compressed <- compressed[Seq,]
  }
  Explan <- NULL
  for(i in seq(nrow(compressed))) {
    ## set explanation
    Explan <- setExplan(compressed[i,], Explan)
    ## set drawing characteristics
    sel <- current$name == compressed$name[i]
    current$lwd[sel] <- Explan$current$lwd
    current$lty[sel] <- Explan$current$lty
    current$pch[sel] <- Explan$current$pch
    current$csi[sel] <- Explan$current$csi
    current$cex[sel] <- Explan$current$csi/par('csi')
    current$col[sel] <- Explan$current$col
  }
  return(list(current=current, Explan=Explan))
}
