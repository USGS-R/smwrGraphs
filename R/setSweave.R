# set up page for USGS illustrations in a PDF file for Sweave
#
# Coding history:
#    2012Aug10 DLLorenz Original Coding from setPDF
#    2012Nov01 DLLorenz Add options to produce circles in PDF output
#    2012Nov01          This version.
#

setSweave <- function(name, width, height, ...) {
  ## Notes: This function returns the device number and name
  ## Set up defaults
  PDFFont <- "Helvetica-Narrow"
  ## set global variable sfor lineweights dpf
  assign(".lwt_factor", 1.0, envir = .GlobalEnv)
  assign(".pdf_graph", TRUE, envir = .GlobalEnv)
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
