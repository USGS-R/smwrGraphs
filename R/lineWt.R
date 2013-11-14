# Return the line weight to be used for graph frames and ticks
# Also the standard line weight
#
# Coding History:
#    2008Aug04 DLLorenz Original coding, assuming WMF from S-PLUS 7.
#    2010Nov16 DLLorenz Modified for R (lineweights based on 1 = 1/96
#    2011Oct24 DLLorenz Tweak for R check
#    2010Oct24          This version.
#

frameWt <- function() {
  if(exists(".lwt_factor"))
    return(2/3 * get(".lwt_factor"))
  else
    return(2/3)
}

stdWt  <- function(x=1) {
  if(exists(".lwt_factor"))
    return(1.06666667 * x * get(".lwt_factor"))
  else
    return(1.06666667 * x)
}

lineWt <- function(x) {
  lwid <- c(stdWt(c(1,4/3, 2)), frameWt())
  names(lwid) <- c("standard", "color", "bold", "hairline") # from setExplan
  return(lwid[x])
}
