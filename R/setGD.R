#' @rdname setPage
#' @export setGD
setGD <- function(name="USGS") {
	# Coding history:
	#    2013Apr09 DLLorenz Original Coding
	#    2014Jun26 DLLorenz Converted to roxygen.
  ##
  ## Protect against overridden default graphics device
  if(exists("windows", mode="function")) 
    setPage("sq", name=name, multiple=TRUE, device="windows")
  else if(exists("quartz", mode="function"))
    setPage("sq", name=name, device="quartz")
  else
    setPage("sq", name=name, device="X11")
  invisible()
}
