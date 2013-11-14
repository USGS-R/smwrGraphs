# Remove leading and trailing spaces from a character vetor
#
# Coding history:
#    2010Nov15 DLLorenz Original Coding
#    2010Nov15          This version.
#
# Note this should be in core

strip.blanks <- function(x) {
  x <- format(x)
  x <- sub('^[ ]+', '', x)
  x <- sub('[ ]+$', '', x)
  return(x)
}
