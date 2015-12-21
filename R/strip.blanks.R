#' Remove Spaces
#' 
#' Removes leading and trailing blanks from a character string.
#' 
#' 
#' @param x a character vector.
#' @return A vector like \code{x}, but with leading and trailing spaces removed
#' from each element.
#' @seealso 
#Flip for production/manual
#'\code{link[base]{sub}}
#\code{base} (in base package)
#' @keywords manip
#' @examples
#' 
#' strip.blanks("   keep me   ")
#' 
#' @export strip.blanks
strip.blanks <- function(x) {
	# Coding history:
	#    2010Nov15 DLLorenz Original Coding
	#    2014Jun26 DLLorenz Converted to roxygen
	#
  x <- format(x)
  x <- sub('^[ ]+', '', x)
  x <- sub('[ ]+$', '', x)
  return(x)
}
