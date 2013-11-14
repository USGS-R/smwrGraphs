# quick set up for graphics page
#
# Coding history:
#    2013Apr09 DLLorenz Original Coding
#

setGD <- function(name="USGS") {
  ## Notes: This function is designed to set up a square garphics
  ## page. It is intended for a quick page setup for any function
  ## that needs one rather than for general use. It can also be
  ## used to set up a page to ensure that the environment is
  ## set up correctly.
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
