#' @rdname setPage
#' @export setPage
setRStudio <- function() {
	# Set up simple RStudio graphics device
	#
	options(.lwt_factor = 1)
	options(.pdf_graph = FALSE)
	wfont <- "Arial Narrow"
	qfont <- paste("Arial Narrow", c("", " Bold", " Italic",
																	 " Bold Italic"), sep="")
	xfont <- "Helvetica"
	Strikes <- 0L
	Windoze <- TRUE
	if(!exists("windows", mode="function")) {
		Windoze <- FALSE
		Strikes <- 1L
		windowsFonts <- function(...) return()
		windowsFont <- function(...) return()
	}
	if(!exists("quartz", mode="function")) {
		Strikes <- Strikes + 1L
		quartzFonts <- function(...) return()
		quartzFont <- function(...) return()
	}
	if(!exists("X11", mode="function")) {
		Strikes <- Strikes + 1L
		X11Fonts <- function(...) return()
	}
	## Needed because on Windoze X11 esists, but fonts are set with windowsFonts
	if(!exists("X11Fonts", mode="function"))
		X11Fonts <- function(...) return()
	if(Strikes == 3L) # You're outa here
		stop("Interactive graphics device not found.")
	device <- options("device")$device
	if(is.character(device))
		device <- get(device, mode="function")
	device()
	## Set up the USGS font, It is OK to set up for devices not present
	##  with dummy calls
	windowsFonts(USGS = windowsFont(wfont))
	quartzFonts(USGS = quartzFont(qfont))
	X11Fonts(USGS = X11Fonts(xfont)[[1L]])
	# Final touches
	par(las=1)
	invisible()
}
