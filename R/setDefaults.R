#' Default Values
#' 
#' Sets the default values for control arguments (support function).
#' 
#' 
#' @param current the control parameters specified in the call to the
#' high-level graphing function.
#' @param \dots the default values for each name required for the control
#' parameters.
#' @return The control parameters with defaults substituted for missing names.
#' @keywords dplot
#' @export setDefaults
setDefaults <- function(current=list(), ...) {
	# Coding History:
	#    2011Jun21 DLLorenz Original coding
	#    2014Jun26 DLLorenz Converted to roxygen
	#
  dots <- list(...)
  for(i in names(dots))
    if(is.null(current[[i]]))
      current[[i]] <- dots[[i]]
  return(current)
}
