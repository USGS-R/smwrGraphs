#' Remove Spaces
#' 
#' Remove surrounding spaces from text.
#' 
#' 
#' @param x a character vector.
#' @return A vector like \code{x}, but with leading and trailing spaces removed
#' from each element.
#' @seealso \code{link{sub}}
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
