#' Explanation
#' 
#' Adds the current plot information to explanation information (support
#' function).
#' 
#' 
#' @param current the current plot information.
#' @param old existing explanation information.
#' @return A list having four components:\cr \item{text}{a list having two
#' components:} \+ \item{text}{the description in the explanation, derived from
#' the \code{name} component in \code{Plot}} \+ \item{cex}{the size of the
#' \code{text} to write in the explanation}
#' 
#' \item{lines}{a list having seven components and controloing both lines and
#' points:} \+ \item{type}{the type of plot (specifying points, lines, and so
#' forth)} \+ \item{lwd}{the line weight} \+ \item{lty}{the line type} \+
#' \item{pch}{the symbol} \+ \item{csi}{the size of the symbol in inches} \+
#' \item{cex}{the }the size of the symbol relative to character size \+
#' \item{col}{the color of the plot}
#' 
#' \item{areas}{a list having two components:} \+ \item{fill}{the color of the
#' fill area} \+ \item{borders}{the color of the border}
#' 
#' \item{current}{a list like \code{current} with the defaults set} Each entry
#' in \code{text} must have a corresponding entry in \code{lines} and
#' \code{areas}.
#' @seealso \code{\link{setPlot}}
#' @keywords dplot
#' @export setExplan
setExplan <- function(current, old=NULL) {
	# Coding History:
	#    2008Jun13 DLLorenz Original coding and start of revisions
	#    2010Nov16 DLLorenz Modified for R (symbol recoding)
	#    2011Jun17 DLLorenz Added shaded areas
	#    2012Nov01 DLLorenz Add options to produce circles in PDF output
	#    2014Jun26 DLLorenz Converted to roxygen.
  ##
  ## Define the variables
  ltypes <- c(1,2,3)
  names(ltypes) <- c("solid", "dashed", "dotted")
  opensym <- c(1, 2, 3, 4, 5, 6, 0, 46, 32)
  names(opensym) <- c("circle", "uptri", "plus", "x",
                      "diamond", "downtri", "square", "dot", "none")
  ## Note that these symbols require bg and col, so calls to plot points
  ## need to add bg = col to those calls
  fillsym <- c(21, 24, 23, 25, 22)
  names(fillsym) <- c("circle", "uptri", "diamond", "downtri", "square")
  ## Reset current--only for lines and points, but need fill info
  current.name <- current$name # keep this!
  lineType <- substring(current$what, 1, 1)
  ## Get colors and tweak for PDF output for points
  ## See https://stat.ethz.ch/pipermail/r-help/2007-October/144598.html
  symColor <- current$color
  if(!is.null(.pdg_graph <- options(".pdf_graph")$.pdf_graph))
  	if(lineType == "p" && .pdf_graph) {
  		symColor <- col2rgb(symColor)
  		symColor <- rgb(t(symColor), alpha=254, maxColorValue=255)
  	}
  Filled <- current$filled
  ## Compute symbol size
  ## The nominal factor to convert character size to symbol sizes 
  ## appears to be 2.5
  symSize <- current$size * 2.5
  current <- list(type=lineType,
                  lwd=lineWt(current$width),
                  lty=ltypes[current$type],
                  pch=if(Filled) fillsym[current$symbol] else opensym[current$symbol],
                  csi=symSize,
                  cex=symSize/par('csi'), # Convert to cex units
                  col=symColor,
                  area.color=current$area.color,
                  area.border=current$area.border)
  ## Set linetype for explanation
  if(lineType == 'h')
    lineType <- 'l'
  ## Create or modify explanation
  if(is.null(old)) { # create a new list
    txt <- list(text=current.name, cex=rep(par('cex'), length(current.name)))
    lines <- current # just use this one
    areas <- list(fill=current$area.color, border=current$area.border)
    old <- list()
  }
  else {
    txt <- old$text
    txt$text <- c(txt$text, current.name)
    txt$cex <- c(txt$cex, rep(par('cex'), length(current.name)))
    lines <- old$lines
    lines$type <- c(lines$type, lineType)
    lines$lwd <- c(lines$lwd, current$lwd)
    lines$lty <- c(lines$lty, current$lty)
    lines$pch <- c(lines$pch, current$pch)
    lines$csi <- c(lines$csi, symSize)
    lines$cex <- c(lines$cex, symSize/par('csi'))
    lines$col <- c(lines$col, symColor)
    areas <- old$areas
    areas$fill <- c(areas$fill, current$area.color)
    areas$border <- c(areas$border, current$area.border)
    old$text <- old$lines <- old$ares <- old$current <- NULL
  }
  return(c(list(text=txt, lines=lines, areas=areas, current=current), old))
}
