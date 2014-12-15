# set up defaults for addPlot argument
#
# Coding History:
#    2008Jun13 DLLorenz Original coding and start of revisions
#    2010Nov16 DLLorenz Modified for R (none required)
#    2011Apr16 DLLorenz Added complete complement of args
#    2011Jun16 DLLorenz Added args for areas
#    2011Oct24 DLLorenz Tweaks for package
#    2011Dec15 DLLorenz Fix for Plot$color = "Auto," required for group
#    2012Nov11 DLLorenz Added "none" options for type
#    2014Feb17 DLLorenz Added/Changed lineweight from standard to color
#



#' Plot Parameters
#' 
#' Set the control parameters for a plot (support function).
#' 
#' The value for \code{what} must be one of: \describe{ \item{"points"}{symbols
#' only,} \item{"lines"}{lines only,} \item{"both"}{lines connecting symbols
#' with a small gap,} \item{"overlaid"}{lines connecting symbols,}
#' \item{"stairstep"}{horizontal line to next x value with a vertical line to
#' the y value,} \item{"vertical"}{vertical lines from the y-axis only.}
#' \item{"none"}{draw nothing} } The value for \code{symbol} must be one of:
#' \describe{ \item{"circle"}{an open circle or filled, depending on
#' \code{filled},} \item{"uptri"}{an open up pointing triangle or filled,
#' depending on \code{filled},} \item{"plus"}{a plus sign (never filled),}
#' \item{"x"}{an x (never filled),} \item{"diamond"}{an open diamond shape or
#' filled, depending on \code{filled},} \item{"downtri"}{an open down pointing
#' triangle or filled, depending on \code{filled},} \item{"square"}{an open
#' square or filled, depending on \code{filled},} \item{"dot"}{a very small dot
#' (never filled),} \item{"+"}{a plus sign (never filled),} \item{"none"}{no
#' symbol or line.} }
#' 
#' @param current list containing the current or those requested by the user.
#' @param name the name of the object plotted; used in the explanation.
#' @param what what to plot, see \bold{Details}.
#' @param type the line type, if drawn, must be one of "solid," "dashed," or
#' "dotted."
#' @param width the width of the line, if drawn, must be one of "standard;"
#' "color," a little wider than "standard;" "bold," twice as wide as
#' "standard;" or "hairline."
#' @param symbol the symbol to plot, if drawn, see \bold{Details}.
#' @param filled if a symbol is drawn, fill with solid color?
#' @param size the size of the symbol, in incehs, if drawn.
#' @param color the color of the symbol or line.
#' @param area.color the color of a shaded area, required for completeness.
#' @param area.border the boundary color of a shaded area, required for
#' completeness.
#' @return A list like \code{current}, but with the defaults supplied for any
#' missing component.
#' @seealso \code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{qqPlot}},
#' \code{\link{piperPlot}}, \code{\link{probPlot}}, \code{\link{colorPlot}}
#' @keywords hplot
#' @export setPlot
setPlot <- function(current, name="", what="lines", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black", area.color=NA, area.border=NA) {
  ## set defaults from args if anything is missing
  if(is.null(current$name))
    current$name <- name
  current$name <- as.character(current$name)
  if(is.null(current$what))
    current$what <- what
  else
    current$what <- match.arg(current$what, c("points", "lines", "both", "overlaid",
                                              "stairstep", "vertical", "none"))
  if(current$what == "vertical")
    current$what <- "h" # this is what is needed for par
  if(is.null(current$type))
    current$type <- type
  else
    current$type <- match.arg(current$type, c("solid", "dashed", "dotted"))
  if(is.null(current$width))
    current$width <- 'standard'
  else
    current$width <- match.arg(current$width, c("standard", "color", "bold", "hairline"))
  if(is.null(current$symbol))
    current$symbol <- symbol
  else
    current$symbol <- match.arg(current$symbol, c("circle", "uptri", "plus", "x",
                                                  "diamond", "downtri", "square", "dot",
                                                  "+", "none"))
  if(current$symbol == "+")
    current$symbol <- "plus" # fix an alternate
  if(is.null(current$filled))
    current$filled <- filled
  if(current$filled && current$symbol %in% c("plus", "x", "dot", "none")) # fix it
    current$filled <- FALSE
  if(is.null(current$size))
    current$size <- size
  if(is.null(current$color) || current$color == "Auto")
    current$color <- color
  if(is.null(current$area.color))
    current$area.color <- area.color
  if(is.null(current$area.border))
    current$area.border <- area.border
  # Check if lines drawn, color not black and lineweight standard
  # if so, then change lineweight to color
  if(current$what %in% c("lines", "both", "overlaid",
  											 "stairstep", "vertical") && 
  	 	current$color != "black" && current$width == "standard")
  	current$width <- "color"
  return(current)
}
