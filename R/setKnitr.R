#' @rdname setPage
#' @export setKnitr
setKnitr <- function(name, width, height, ...) {
	if(missing(name)) { # only reset graphics parameters, needed for knitr
		fig <- par("din")
		par(fin=c(fig[1L]-.5, fig[2L]-.5), las=1)
		if(all(names(pdfFonts()) != "USGS")) # Check to see if already in the PDF font list
			pdfFonts("USGS" = Type1Font("Helvetica-Narrow",
																	pdfFonts("Helvetica-Narrow")[[1]]$metrics))
	} else {
		## Set up defaults
		PDFFont <- "Helvetica-Narrow"
		## set global variable sfor lineweights dpf
		options(.lwt_factor = 1)
		options(.pdf_graph = TRUE)
		fontSize <- 8
		font <- PDFFont
		pdf(file=name, onefile=TRUE, width=width, height=height,
				family=PDFFont, pointsize=8, colormodel="rgb", title=name)
		## set up for export to PDF file.
		if(all(names(pdfFonts()) != "USGS")) # Check to see if already in the PDF font list
			pdfFonts("USGS" = Type1Font("Helvetica-Narrow",
																	pdfFonts("Helvetica-Narrow")[[1]]$metrics))
		dev <- dev.cur()
		par(fin=c(width-.5, height-.5), las=1)
	}
	invisible()
}
