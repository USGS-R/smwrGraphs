#' @rdname setPage
#' @export setPDF
setPDF <- function(layout="portrait", basename="USGS", multiplefiles=FALSE) {
  ## 
  ## set global variables for lineweights and pdf
  options(.lwt_factor = 1)
  options(.pdf_graph = TRUE)
  fontSize <- 8
  if(class(layout) == "list") { # custom
    width <- layout$wid
    height <- layout$hei
    if("fin" %in% names(layout)){
      fin <- layout$fin
    } else {
      fin <- c(width, height) - .1
    }
    
  }
  else {
    layout=match.arg(layout, c("portrait", "landscape"))
    if(layout == "portrait") {
      fin <- c(7 + 1/6, 9 + 1/3)
      width=8.5
      height=11.
    }
    else { # landscape
      fin <- c(9 + 1/3, 7 + 1/6)
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
  PDFFont <- "Helvetica-Narrow"
  pdf(file=name, onefile=!multiplefiles, width=width, height=height,
      family=PDFFont, pointsize=fontSize, colormodel="cmyk", title=basename)
  ## set up for export to PDF file.
  if(all(names(pdfFonts()) != "USGS")) # Check to see if already in the PDF font list
    pdfFonts("USGS" = Type1Font("Helvetica-Narrow",
             pdfFonts("Helvetica-Narrow")[[1L]]$metrics))
  dev <- dev.cur()
  par(fin=fin, las=1)
  invisible(list(dev=dev, name=basename))
}
