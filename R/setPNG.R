#' @rdname setPage
#' @export setPNG
setPNG <- function(name, width, height, ...) {
	
  Windoze <- TRUE
  Strikes <- 0L
  pngType <- "win"
  wfont <- "Arial Narrow"
  qfont <- paste("Arial Narrow", c("", " Bold", " Italic",
                                   " Bold Italic"), sep="")
  xfont <- "Helvetica"
  if(!exists("windows", mode="function")) {
    Windoze <- FALSE
    Strikes <- 1L
    windowsFonts <- function(...) return()
    windowsFont <- function(...) return()
    pngType <- getOption("bitmapType")
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
  windowsFonts(USGS = windowsFont(wfont))
  quartzFonts(USGS = quartzFont(qfont))
  X11Fonts(USGS = X11Fonts(xfont)[[1L]])
  
  if(missing(name)) { # only reset graphics parameters, needed for knitr
		fig <- par("din")
		par(fin=c(fig[1L]-.5, fig[2L]-.5), las=1)
		windowsFonts(USGS=windowsFont("Arial Narrow"))
		postscriptFonts(USGS=postscriptFonts()[["Helvetica-Narrow"]])		  


	} else {
		print(c(width=width, height=height))
		print(paste("Setting up markdown graphics device: ", name))
		## Set up defaults
		
		PNGFont <- "Arial Narrow"
		## set global variable sfor lineweights dpf
		options(.lwt_factor = 1)
		options(.pdf_graph = TRUE)
		png(filename=name, width=width, height=height, units="in",
				res=300, family=PNGFont, pointsize=8, type=pngType)
		windowsFonts(USGS=windowsFont("Arial Narrow"))
		## set up for export to PNG file.
		dev <- dev.cur()
		par(fin=c(width-.5, height-.5), las=1)
	}
	invisible()
}
