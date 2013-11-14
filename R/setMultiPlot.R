# Sets up parameters and explanation for graphs that can have multiple
#  symbols plotted at one time, such as piperPlot and timePlot
#
# Coding history:
#    2008Oct27 DLLorenz Original Code and begin of tweaks
#    2011Jan12 DLLorenz Conversion to R
#    2011Apr16 DLLorenz Added complete complement of args to setPlot
#    2011Jun14 DLLorenz Added option to set the order in the explanation
#    2011Jun16 DLLorenz Added empty info for areas
#    2011Oct24 DLLorenz Tweaks for package
#    2011Dec15          This version.
#

setMultiPlot <- function(current, Nobs=1, name="", what='points', type='solid',
                         width='standard', symbol='circle', filled=TRUE,
                         size=0.09, color='black', order='as is') {
  ## Arguments:
  ##  ...
  ##  order (character scalar or vector) scalar can be 'as is'--do nothing to order
  ##    in explanation, 'sort' or 'increasing'--put into sorted order, 'decreasing'--
  ##    put in reverse order; vector specifies the exact order
  ## first set up current to have all components with correct length
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
