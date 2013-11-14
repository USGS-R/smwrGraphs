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
#    2012Nov11         This version.
#

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
  return(current)
}
