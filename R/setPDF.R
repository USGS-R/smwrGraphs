#' @rdname setPage
#' @export setPDF
setPDF <- function(layout="portrait", basename="USGS", multiplefiles=FALSE) {
	# Coding history:
	#    2011Jan26 DLLorenz Original Coding from setPage
	#    2011Oct24 DLLorenz Tweaks for package
	#    2011Nov17 DLLorenz Bug fixes for pdfFonts call and layout
	#    2012Sep12 DLLorenz Changed buffer around the graph to .1 inch for custom
	#                       size to facilitate making graphs fill the page as much
	#                       as possible.
	#    2012Nov01 DLLorenz Add options to produce circles in PDF output
	#    2014Jun26 DLLorenz Converted to roxygen.
	#
  ## Set up defaults
  PDFFont <- "Helvetica-Narrow"
  ## set global variables for lineweights and pdf
  options(.lwt_factor = 1)
  options(.pdf_graph = TRUE)
  fontSize <- 8
  font <- PDFFont
  if(class(layout) == "list") { # custom
    width <- layout$wid
    height <- layout$hei
    fin <- c(width, height) - .1
  }
  else {
    layout=match.arg(layout, c("portrait", "landscape"))
    if(layout == "portrait") {
      fin <- c(7.25, 9.5)
      width=8.5
      height=11.
    }
    else { # landscape
      fin <- c(9.5, 7.25)
      width=11.
      height=8.5
    }
  }
  name <- make.names(basename) # make a legal name
  ## set up the output
  if(multiplefiles) # make name
    name <- paste(name, "%03d.pdf", sep="")
  else
    name <- paste(name, ".pdf", sep="")
  pdf(file=name, onefile=!multiplefiles, width=width, height=height,
      family=PDFFont, pointsize=8, colormodel="cmyk", title=basename)
  ## set up for export to PDF file.
  if(all(names(pdfFonts()) != "USGS")) # Check to see if already in the PDF font list
    pdfFonts("USGS" = Type1Font("Helvetica-Narrow",
             pdfFonts("Helvetica-Narrow")[[1L]]$metrics))
  dev <- dev.cur()
  par(fin=fin, las=1)
  invisible(list(dev=dev, name=basename))
}
