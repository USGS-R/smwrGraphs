#' Graphics Page
#' 
#' Sets up a graphics page.
#' 
#' If \code{layout} is "portrait," then the page size is 8.5 by 11 inches and
#' the graph area is 7.25 by 9.5 inches.\cr If \code{layout} is "landscape,"
#' then the page size is 11 by 8.5 inches and the graph area is 9.5 by 7.25
#' inches.\cr If \code{layout} is "square," then the page size is 7 by 7 inches
#' and the graph area is 6.5 by 6.5 inches (\code{setPage} only).\cr If
#' \code{layout} is "slide," then the page size is 10 by 7.5 inches and the
#' graph area is 9.5 by 7.0 inches (\code{setPage} only).\cr \code{Layout} may
#' also be a tagged list, with components \code{width} and \code{height} giving
#' the width and height of the page, the width and height of the graph area is
#' 0.5 inch less that the page, except for \code{setPDF} where it is 0.1 inch
#' less.\cr The user may specify a graphics device other than the default for
#' the system. This may be necessary when running under certain user
#' environments like RStudio (TM).
#' 
#' @rdname setPage
#' @aliases setPage setPDF setSweave setGD
#' @param layout A description of the orientation and shape of the graphics
#' page. See \bold{Details}.
#' @param font a description of the font. The choices are "preview," which is
#' 12 point Arial Narrow; "USGS," which is 8 point Arial Narrow; "EST," which
#' is 8 point Times New Roman; "PPT," which is 24 point Arial; and "PDF," which
#' is 8 point Arial Helvetica-Narrow. "PDF" should be chosen if the graphs are
#' to be saved to a portable document format (pdf) file.
#' @param name the name of the graphics page or the filename for
#' \code{setSweave}.
#' @param multiple allow multiple pages?
#' @param device the name of the graphics device. See \bold{Details}.
#' @param basename the base name of the pdf file name.
#' @param multiplefiles modify \code{basename} to create multiple files for
#' multiple pages?
#' @param height the height of the graphics page.
#' @param width the widht of the graphics page.
#' @param \dots additional arguments, which are ignored by \code{setSweave}.
#' @return For \code{setPage} and \code{setPDF}, a list with two components:
#' dev, the device number; and name, the name or basename. For \code{setSweave}
#' nothing is returned.
#' @note The function \code{setSweave} is an interface to be used when using
#' \code{Sweave}. The functions \code{setSweave} and \code{setPDF} require a
#' call to \code{dev.off} to close the graphics device after all graphics is
#' completed.\cr The function \code{setGD} is designed to be a quick-and-dirty
#' graphics page set up function. It is designed to be used by functions to set
#' up the graphics environment if the user fails to do so.
#' @seealso \code{\link{setLayout}}, \code{\link{setGraph}}
#' @keywords dplot
#' @examples
#' \dontrun{
#' # See for examples of setGD:
#' demo(topic="AnnualFlowBarChart", package="smwrGraphs")
#' demo(topic="Coplot-complexScatterPlot", package="smwrGraphs")
#' demo(topic="Coplot-simpleBoxPlot", package="smwrGraphs")
#' demo(topic="DurationHydrograph", package="smwrGraphs")
#' demo(topic="FlowDur-Measurements", package="smwrGraphs")
#' demo(topic="HydroPrecip", package="smwrGraphs")
#' # See for examples of setPage:
#' demo(topic="PiperScript", package="smwrGraphs")
#' # See for examples of setPDF:
#' demo(topic="MeasurementRating", package="smwrGraphs")
#' demo(topic="PiperScript", package="smwrGraphs")
#' demo(topic="RightAxisExample", package="smwrGraphs")
#' demo(topic="TopAxisExample", package="smwrGraphs")
#' # See for examples of setSweave:
#' vignette(topic="BoxPlots", package="smwrGraphs")
#' vignette(topic="DateAxisFormats", package="smwrGraphs")
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' vignette(topic="GraphSetup", package="smwrGraphs")
#' vignette(topic="LineScatter", package="smwrGraphs")
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' vignette(topic="ProbabilityPlots", package="smwrGraphs")
#' }
#' @export setPage
setPage <- function(layout="portrait", font="preview", name="USGS",
                    multiple=FALSE, device="default") {
	# Coding history:
	#    2008May12 DLLorenz Original Coding and start of revisions.
	#    2009Dec01 DLLorenz Added EST option, which apparently requires Times New Roman
	#    2010Feb02 DLLorenz Convert to use current graphics
	#    2010Mar28 DLLorenz Modified extensively for new interface
	#    2010Nov15 DLLorenz Modified for R
	#    2011Aug03 DLLorenz Added slide and custom options
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Feb14 DLLorenz Modifications for all platforms
	#    2012Mar12 DLLorenz Added device argument to override some default devices
	#                       as with RStudio
	#    2012Nov01 DLLorenz Add options to produce circles in PDF output
	#    2014Jun26 DLLorenz Converted to roxygen.
	#
  font <- match.arg(font, c("preview", "USGS", "EST", "PPT", "PDF"))
  ## Set global variables for lineweights and PDF
  if(font == "PPT")
  	options(.lwt_factor = 2)
  else
  	options(.lwt_factor = 1)
  options(.pdf_graph = FALSE)
  fontSize <- switch(font, 
                     preview=12,
                     USGS=8,
                     EST=8,
                     PPT=24,
                     PDF=8)
  wfont <- switch(font,
                  preview="Arial Narrow",
                  USGS="Arial Narrow",
                  EST="Times New Roman",
                  PPT="Arial",
                  PDF="Helvetica-Narrow")
  qfont <- switch(font,
                  preview=paste("Arial Narrow", c("", " Bold", " Italic",
                    " Bold Italic"), sep=""),
                  USGS=paste("Arial Narrow", c("", " Bold", " Italic",
                    " Bold Italic"), sep=""),
                  EST=paste("Times", c("-Roman", "-Bold", "-Italic",
                    "-Bold-Italic"), sep=""),,
                  PPT=paste("Arial", c("", " Bold", " Italic",
                    " Bold Italic"), sep=""),
                  PDF=paste("Helvetica", c("", "-Bold", "-Oblique",
                    "-BoldOblique"), sep=""))
  xfont <- switch(font,
                  preview="Helvetica",
                  USGS="Helvetica",
                  EST="Times",
                  PPT="Helvetica",
                  PDF="Helvetica")
  if(is.character(layout)) {
    layout <- match.arg(layout, c("portrait", "landscape", "square", "slide"))
    if(layout == "portrait") {
      fin <- c(7.25, 9.5)
      width=8.5
      height=11.
    }
    else if(layout == "landscape"){
      fin <- c(9.5, 7.25)
      width=11.
      height=8.5
    }
    else if(layout == "slide") {
      fin <- c(9.5, 7.0)
      width=10.
      height=7.5
    }
    else { # square, useful for diagnostic plots
      fin <- c(6.5, 6.5)
      width=7.
      height=7.
    }
  } # End of character layout
  else { # must be named vector or tagged list
    width <- layout[["width"]]
    height <- layout[["height"]]
    fin <- c(width, height) - 0.5
  }
  name <- make.names(name) # make a legal name
  ## Set up the graph sheet
  ## Erase what is there for windows (OK for anything else)
  assign(".SavedPlots", NULL, envir=.GlobalEnv)
  ## Get the default graphics device
  if(device == "default")
    device <- options("device")$device
  if(is.character(device))
    device <- get(device, mode="function")
  ## Best guess for the default graphics device and set 
  Strikes <- 0
  Windoze <- TRUE
  if(!exists("windows", mode="function")) {
    Windoze <- FALSE
    Strikes <- 1
    windowsFonts <- function(...) return()
    windowsFont <- function(...) return()
  }
  if(!exists("quartz", mode="function")) {
    Strikes <- Strikes + 1
    quartzFonts <- function(...) return()
    quartzFont <- function(...) return()
  }
  if(!exists("X11", mode="function")) {
    Strikes <- Strikes + 1
    X11Fonts <- function(...) return()
  }
  ## Needed because on Windoze X11 esists, but fonts are set with windowsFonts
  if(!exists("X11Fonts", mode="function"))
    X11Fonts <- function(...) return()
  if(Strikes == 3) # You're outa here
    stop("Interactive graphics device not found.")
  if(Windoze)
    device(width=width, height=height, pointsize=fontSize, rescale='fixed',
           title=name, record=multiple)
  else
    device(width=width, height=height, pointsize=fontSize, title=name)
  ## Set up the USGS font, It is OK to set up for devices not present
  ##  with dummy calls
  windowsFonts(USGS = windowsFont(wfont))
  quartzFonts(USGS = quartzFont(qfont))
  X11Fonts(USGS = X11Fonts(xfont)[[1L]])
  ## Set up for possible export to PDF file (currently only Windoze)
  if(!('USGS' %in% names(pdfFonts()))) # do it only once!
    pdfFonts('USGS' = Type1Font("Helvetica-Narrow",
               pdfFonts("Helvetica-Narrow")[[1L]]$metrics))
  dev <- dev.cur()
  ## Check to make sure fin is OK
  plot.new
  Maxfin <- par("fin")
  par(fin=pmin(fin, Maxfin), las=1)
  invisible(list(dev=dev, name=name))
}
