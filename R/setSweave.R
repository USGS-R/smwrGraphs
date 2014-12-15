#' @rdname setPage
#' @export setSweave
setSweave <- function(name, width, height, ...) {
	# Coding history:
	#    2012Aug10 DLLorenz Original Coding from setPDF
	#    2012Nov01 DLLorenz Add options to produce circles in PDF output
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  ## Set up defaults
  PDFFont <- "Helvetica-Narrow"
  ## set global variable sfor lineweights dpf
  options(.lwt_factor = 1)
  options(.pdf_graph = TRUE)
  fontSize <- 8
  font <- PDFFont
  fname <- paste(name, ".pdf", sep="")
  pdf(file=fname, onefile=TRUE, width=width, height=height,
      family=PDFFont, pointsize=8, colormodel="cmyk", title=name)
  ## set up for export to PDF file.
  if(all(names(pdfFonts()) != "USGS")) # Check to see if already in the PDF font list
    pdfFonts("USGS" = Type1Font("Helvetica-Narrow",
             pdfFonts("Helvetica-Narrow")[[1]]$metrics))
  dev <- dev.cur()
  par(fin=c(width-.5, height-.5), las=1)
  invisible()
}
