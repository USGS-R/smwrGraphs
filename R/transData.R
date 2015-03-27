#' Transform Data
#' 
#' Transform numeric data to match any axis transform (support function).
#' 
#' 
#' @param data data for axis
#' @param logT logical, if \code{TRUE}, then log transform \code{data}
#' @param revT logical, if \code{TRUE}, then reverse \code{data} to match the axis.
#' @param trans arbitrary transform function to apply to \code{data}.
#' @param transarg list of arguments to \code{trans}.
#' @return A vector like \code{data} transformed to plot correctly on an axis.
#' @seealso \code{\link{transPlot}}, \code{\link{probPlot}}
#' @keywords dplot
#' @export transData
transData <- function(data, logT=FALSE, revT=FALSE, trans=as.vector, transarg=NULL) {
	# Coding History:
	#    2008Jul03 DLLorenz Original coding.
	#    2010Nov20 DLLorenz Begin modifications for R
	#    2014Jun27 DLLorenz Converted to roxygen
  ##
  data <- numericData(data) # force to double
  if(is.na(logT)) { # NA means use transform function
    assign('xtrans', trans)
    m <- c(list(as.name('xtrans'), data), transarg)
    m <- as.call(m)
    data <- eval(m)
  }
  else if(logT)
    data <- log10(data)
  if(revT)
    data <- -data
  return(data)
}
