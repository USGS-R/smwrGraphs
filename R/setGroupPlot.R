# Sets up parameters and explanation for graphs that have lines or
#  symbols for groups of data like ecdfPlot
#
# Coding history:
#    2011Dec15 DLLorenz Original Code and begin of tweaks
#    2011Dec15          This version.
#

setGroupPlot <- function(current, Grps=1, name="", what='points', type='solid',
                         width='standard', symbol='circle', filled=TRUE,
                         size=0.09, color='black') {
  ## Arguments:
  ##  ...
  ##  Grps, the number of groups
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
