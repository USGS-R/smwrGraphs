#' @rdname setPage
#' @export setPNG
setPNG <- function(name, width, height, ...) {
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
				res=300, family=PNGFont, pointsize=8, type="windows")
		windowsFonts(USGS=windowsFont("Arial Narrow"))
		## set up for export to PNG file.
		dev <- dev.cur()
		par(fin=c(width-.5, height-.5), las=1)
	}
	invisible()
}
