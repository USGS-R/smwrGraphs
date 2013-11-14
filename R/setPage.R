# set up page for USGS illustrations
#
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
#    2012Nov01          This version.
#

setPage <- function(layout="portrait", font="preview", name="USGS",
                    multiple=FALSE, device="default") {
  ## Notes: This function returns the device number and name
  font <- match.arg(font, c("preview", "USGS", "EST", "PPT", "PDF"))
  ## Set global variables for lineweights and PDF
  if(font == "PPT")
    assign(".lwt_factor", 2.0, envir = .GlobalEnv)
  else
    assign(".lwt_factor", 1.0, envir = .GlobalEnv)
  assign(".pdf_graph", FALSE, envir = .GlobalEnv)
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
