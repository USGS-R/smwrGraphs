#' Plot Parameters
#' 
#' Set plotting controls for groups of data (support function).
#' 
#' If the \code{name} component in the original call to the high-level plot is
#' "Auto," then the description for the explanation is taken from the value in
#' the \code{Group} argument in that call. Otherwise the user must specify a
#' name for each group.\cr If the \code{color} component in the original call
#' to the high-level plot is "Auto," then the colors for each group are based
#' on a sequence of 15 colors that are easily distinguished from each other. If
#' there are more than 15 groups, then a gray scale is used with no guarantee
#' if easily distinguished colors.
#' 
#' @param current the plot parameters specified in the call to the high-level
#' graphing function.
#' @param Grps the number of groups.
#' @param name the name associated with the group. See \bold{Details}.
#' @param what what kind of plot. Must be one of "points," symbols only;
#' "lines," line segments connecting points only; "both," line segments
#' connecting isolated symbols; "overlaid," line segments connecting points
#' with symbols; "stairstep," stairstep line segments; or "vertical," vertical
#' lines from the y-axis origin to the y value at each x value.
#' @param type the type of line, if drawn. Must be one of "solid," "dashed,"
#' "dotted."
#' @param width the width of line, if drawn. Must be one of "standard,"
#' resulting in a line width of about .8 points; "color," resulting in a line
#' width of about 1 points; "bold," resulting in a line width of about 1.6
#' points; or "hairline" resulting in a line width of about .5 points. Note
#' these values are doubled if the \code{font} argument to \code{setPage} is
#' "PPT."
#' @param symbol type symbol, if drawn. Must be one of "circle;" "uptri,"
#' upward pointing triangle; "plus;" "x;" "diamond;" "downtri," downward
#' pointing triangle; "square;" or "dot."
#' @param filled fill the symbol? Valid only for \code{symbol} equal to
#' "circle," "uptri," "diamond," "downtri," or "square."
#' @param size the size of the symol in inches, if drawn.
#' @param color the color of the plotted values for each group. Can be a named
#' color, such as "black" or "gray50" or an RGB color like "#A09623."
#' @return A list having two components:\cr \item{current}{a list like
#' \code{current} with the defaults set} \item{Explan}{a list for creating an
#' explanation}
#' @seealso \code{\link{setExplan}}, for details about the list required for an
#' explanation.
#' @keywords dplot
#' @export setGroupPlot
setGroupPlot <- function(current, Grps=1, name="", what='points', type='solid',
                         width='standard', symbol='circle', filled=TRUE,
                         size=0.09, color='black') {
	# Coding history:
	#    2011Dec15 DLLorenz Original Code and begin of tweaks
	#    2014Jun26 DLLorenz Converted to roxygen.
  ##
  ## An useful sequence of 15 colors
  Colors <- c("black", "red", "green", "blue", "gray50", "magenta",
              hsv(h=c(0, 1/12, 1/6, 1/3, 1/2, 3/12, 1/12, 7/12, 5/6),
                  v=c(.5, 1, .5, .5, .75, .75, .5, .5, .5)))
  ## First set up current to have all components with correct length
  if(is.null(current$name) || current$name[1] == "Auto")
    current$name <- name
  current$name <- rep(as.character(current$name), length.out=Grps)
  
  if(is.null(current$what))
    current$what <- what
  current$what <- rep(current$what, length.out=Grps)
  current$what <- pmatch(current$what, c("points", "lines", "both", "overlaid",
                                         "stairstep", "vertical"), nomatch=1,
                         duplicates.ok=TRUE)
  current$what <- c("p", "l", "b", "o", "s", "h")[current$what]
  ## What (type argument) stays as what
  if(is.null(current$type))
    current$type <- type
  current$type <- rep(current$type, length.out=Grps)
  current$type <- pmatch(current$type, c("solid", "dashed", "dotted"), nomatch=1,
                         duplicates.ok=TRUE)
  current$type <- c("solid", "dashed", "dotted")[current$type]
  current$lty <- rep(1, Grps) ## required for lty argument
  
  if(is.null(current$width))
    current$width <- width
  current$width <- rep(current$width, length.out=Grps)
  current$width <- pmatch(current$width, c("standard", "color", "bold", "hairline"),
                          nomatch=1, duplicates.ok=TRUE)
  current$width <- c("standard", "color", "bold", "hairline")[current$width]
  current$lwd <- rep(1, Grps) ## required for lwd argument
  
  if(is.null(current$symbol))
    current$symbol <- symbol
  current$symbol <- rep(current$symbol, length.out=Grps)
  current$symbol <- pmatch(current$symbol, c("circle", "uptri", "plus", "x",
                                             "diamond", "downtri", "square", "dot",
                                             "+", "none"), nomatch=1,
                           duplicates.ok=TRUE)
  current$symbol <- c("circle", "uptri", "plus", "x", "diamond", "downtri", 
                      "square", "dot", "plus", "none")[current$symbol]
  current$pch <- rep(1, Grps) ## required for pch argument
  
  if(is.null(current$filled))
    current$filled <- filled
  current$filled <- rep(current$filled, length.out=Grps)
  current$filled <- ifelse(current$filled & current$symbol %in%
                           c("plus", "x", "dot", "none"), 
                           FALSE, current$filled) # fix it
  
  if(is.null(current$size))
    current$size <- size
  current$size <- rep(current$size, length.out=Grps)
  current$csi <- rep(1.5, Grps) ## required for csi argument
  
  if(is.null(current$color)) {
    current$color <- color
    current$color <- rep(current$color, length.out=Grps)
  }
  if(current$color[1] == "Auto") {
    if(Grps <= 15)
      current$color <- Colors[seq(Grps)]
    else
      current$color <- gray(seq(0, (Grps - 1)/Grps, length.out=Grps)) # Punt
  }
  current$col <-current$color # Not sure what $col is needed
  ## Add the empty area info
  current$area.color <- rep(NA, length.out=Grps)
  current$area.border <- rep(NA, length.out=Grps)
  ## OK now, we've got everything filled out, make an explanation and code symbols
  compressed <- as.data.frame(current,stringsAsFactors=FALSE)
  Explan <- NULL
  for(i in seq(nrow(compressed))) {
    ## set explanation
    Explan <- setExplan(compressed[i,], Explan)
  }
  return(list(current=current, Explan=Explan))
}
