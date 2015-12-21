#' @title Line Weights
#' 
#' @description Computes the weight, or width, of a line. Used primarily 
#'as a support function.
#' 
#' @aliases lineWt stdWt frameWt
#' @param x for \code{lineWt}, one of "standard" (0.7 pt), "color" (0.8 pt),
#'"bold" (1.0 pt), or "hairline" (0.5 pt). For \code{stdWt}, a multiplier 
#'to set the line weight.
#' 
#' @return The width of the line to set as the \code{lwd} graphics parameter.
#' @seealso 
#Flip for production/manual
#'\code{\link{par}}
#\code{par} (in graphics package)
#' @keywords dplot
#' @export lineWt
lineWt <- function(x) {
	# Coding History:
	#    2008Aug04 DLLorenz Original coding, assuming WMF from S-PLUS 7.
	#    2010Nov16 DLLorenz Modified for R (lineweights based on 1 = 1/96
	#    2011Oct24 DLLorenz Tweak for R check
	#    2014Jun26 DLLorenz Converted to roxygen
	#
  lwid <- c(stdWt(c(1, 8/7, 10/7)), frameWt())
  names(lwid) <- c("standard", "color", "bold", "hairline") # from setExplan
  return(lwid[x])
}

#' @rdname lineWt
#' @export frameWt
frameWt <- function() {
	if(!is.null(.lwt_factor <- options(".lwt_factor")$.lwt_factor)) {
		return(2/3 * .lwt_factor)
	} else
		return(2/3)
}

#' @rdname lineWt
#' @export stdWt
stdWt  <- function(x=1) {
	if(!is.null(.lwt_factor <- options(".lwt_factor")$.lwt_factor)) {
		return(14/15 * x * .lwt_factor)
	}	else
		return(14/15 * x)
}
