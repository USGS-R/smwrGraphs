# set up defaults for lists
#
# Coding History:
#    2011Jun21 DLLorenz Original coding
#    2011Jun21          This version.
#

setDefaults <- function(current=list(), ...) {
  dots <- list(...)
  for(i in names(dots))
    if(is.null(current[[i]]))
      current[[i]] <- dots[[i]]
  return(current)
}
