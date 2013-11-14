# Convert anything to consistent numeric values
#
# Coding History:
#    2011Jan08 DLLorenz Original coding.
#    2012Oct26 DLLorenz Fix to adjust POSIXt dates to local time
#    2012Jan04          This version
#

numericData <- function(x, lev=NULL) {
  if(isDateLike(x)) {
    x.Date <- as.Date(x)
    x.temp <- as.POSIXlt(x)
    attr(x.temp, "tzone") <- "GMT"
    x.temp <- as.double(x.temp) - as.double(as.POSIXlt(x.Date))
    x.temp <- x.temp / 86400
    return(as.double(x.Date + x.temp))
  }
  if(isCharLike(x)) {
    if(is.factor(x)) {
      if(is.null(lev)) {
        lev <- levels(x)
        ##        print(lev)
      }
      x <- as.character(x)
    }
    else { # Must be character
      if(is.null(lev))
        return(as.double(x)) # if levels is null, treat as numeric
    }
    return(as.double(factor(x, levels=lev)))
  }
  ## Anything else?
  return(as.double(x))
}
