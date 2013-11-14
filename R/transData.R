# transform data for plotting, internal function
#
# Coding History:
#    2008Jul03 DLLorenz Original coding.
#    2010Nov20 DLLorenz Begin modifications for R
#    2013Jun30          This version.
#

transData <- function(data, logT=FALSE, revT=FALSE, trans=as.vector, transarg=NULL) {
  ## arguments:
  ##   data - data for axis
  ##   logT - do a log transform
  ##   revT - reverse data
  ##   trans - arbitrary transform function, like probability
  ##   transarg - list of arguments to trans
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
