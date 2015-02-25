#' Colors
#' 
#' Checks or converts anything to valid colors (support function).
#' 
#' 
#' @param Color any kind of data that might be interpreted as a color.
#' @return The values in \code{Color} converted to a value that could be
#' interpreted as a color.
#' @seealso \code{link{colors}}, \code{link{rainbow}}
#' @keywords dplot
#' @examples
#' \dontrun{
#' # See for examples of setColor:
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' demo(topic="PiperScript", package="smwrGraphs")
#' }
#' @export setColor
setColor <- function(Color) {
	# Coding history:
	#    2011Apr15 DLLorenz Original dated code.
	#    2011Apr17          This version.
	#    2014Jun26 DLLorenz Converted to roxygen
	#
  if(is.numeric(Color))
    return(as.integer(abs(Color)))
  ## should be factor or char, force to char
  Color <- as.character(Color)
  ## Are they color names?
  UniqCol <- unique(Color)
  if(!all(UniqCol %in% colors())) {
    ## Nope, at least one is not a valid color name, check is hex
    if(!all(grepl("^#", UniqCol))
       ) {# Nope, not hexcolors [get parse error if paren is on previous line]
      ## generate a sequence of colors 
      N <- length(UniqCol)
      Cols <- rainbow(N, start=0.05, end=0.55)
      Color <- match(Color, UniqCol) # index to Cols
      Color <- Cols[Color]
    }
  }
  return(Color)
}
