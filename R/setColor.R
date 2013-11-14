# check/convert anything to valid colors (names, Hexvalues, numbers (1-8))
#
# Coding history:
#    2011Apr15 DLLorenz Original dated code.
#    2011Apr17          This version.
#

setColor <- function(Color) {
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
