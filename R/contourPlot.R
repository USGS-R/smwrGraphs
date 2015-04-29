#' Contour Plot
#' 
#' Produce a contour plot or a colored surface with colors corresponding to 
#'values in \code{z}.
#' 
#' Missing values are permitted in \code{z}, \code{x}, and \code{y} for the default
#'method and are removed, with a warning. before constructing the surface.
#'
#'Missing values are not permitted in \code{rows} or \code{columns} but are permitted
#'in \code{z} for the matrix method. Missing values in \code{z} result in blank areas
#'in the plot.
#'
#' @aliases contourPlot contourPlot.default contourPlot.matrix
#' @param z the values representing the surface data.
#' @param x the x-axis coordinates for each value in \code{z}.
#' @param y the y-axis coordinates for each value in \code{z}.
#' @param rows the coordinates for \code{z} represented by the rows in the matrix.
#' @param cols the coordinates for \code{z} represented by the columns in the matrix.
#' @param matrix.rows a single character, either "x" or "y" indicating whether the rows in z should be 
#'plotted along the x or y axis.
#' @param Grid control paramters for gridding irregularly spaced data.
#' @param Contours control parameters for the coutour lines or levels in the filled plot.
#' @param yaxis.range	set the range of the y-axis.
#' @param xaxis.range	set the range of the x-axis.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for details.
#' @param xlabels set up x-axis labels. See \code{\link{linearPretty}} for details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins.
#' @param \dots not used, required for other methods.
#' @return Information about the graph.
#' @importFrom akima interp
#' @examples
#' \dontrun{
#' # See for examples of contourPlot:
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export contourPlot
contourPlot <- function(z, ...)
  UseMethod("contourPlot")
# Coding history:
#    2011Jun21 DLLorenz Initial coding
#    2013Mar08 DLLorenz Completed and added option for x to be of class "Date"
#    2013Apr09 DLLorenz Added setGD 
#    2014Jun25 DLLorenz Converted to roxygen

#' @rdname contourPlot
#' @export
#' @method contourPlot default
## The "default" method--numeric z with x and y coordinates
contourPlot.default <- function(z, x, y, # data specs
                                Grid=list(method="interpolate",
                                  linear=TRUE, extrapolate=FALSE, density=90, # interp options
                                  ## Loess options
                                  span=0.75, degree=1, family="symmetric"), # Grid
                                Contours=list(name="Auto",
                                  levels=10,
                                  filled=FALSE,
                                  lineColor="black",
                                  lineLabel="flattest",
                                  fillColors="coolWarm"), # Contour controls
                                yaxis.range=c(NA,NA), # y-axis controls
                                xaxis.range=c(NA,NA), # x-axis controls
                                ylabels=4, xlabels=4, # axis labels
                                xtitle=deparse(substitute(x)),
                                ytitle=deparse(substitute(y)), # axis titles
                                caption="",# caption
                                margin=c(NA, NA, NA, NA), ...) { # margin control
  ##
  ## Set up the axes titles
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  ## Process the z data: create a matrix from irregular x, y data
  ## Force defaults in Grid
  Grid <- setDefaults(Grid, method='interpolate',
                      linear=T, extrapolate=F, density=90,
                      span=0.75, degree=1, family='symmetric')
  ## Remove missings
  Bad <- is.na(x) | is.na(y) | is.na(z)
  if(any(Bad)) {
    warning(sum(Bad), " missing value(s) removed")
    x <- x[!Bad]
    y <- y[!Bad]
    z <- z[!Bad]
  }
  ## To maintain a consistent scale in x and y scale to a common range
  ## Map xax$range[1] to 0 and xax$range[2] to 10
  ## Note numeric conversion required for dates, recovered below
  xrng <- as.double(range(x))
  yrng <- range(y)
  xs <- scaleRng(as.double(x), Min=0, Max=10, x.range=xrng)
  ys <- scaleRng(y, Min=0, Max=10, x.range=yrng)
  if(Grid$method == "loess") # Create a smooth surface first
    z <- fitted(loess(z ~ xs + ys, span=Grid$span, degree=Grid$degree,
                      family=Grid$family, normalize=F))
  xo <- seq(0, 10, length=Grid$density)
  yo <- seq(0, 10, length=Grid$density)
  zo <- interp(xs, ys, z, xo=xo, yo=yo, linear=Grid$linear,
               extrap=Grid$extrapolate)$z
  ## Scale xo and y back to real-world units
  xo <- scaleRng(xo, Min=xrng[1], Max=xrng[2], x.range=c(0,10))
  yo <- scaleRng(yo, Min=yrng[1], Max=yrng[2], x.range=c(0,10))
  # Convert Date like x to Date
  if(class(x) == "Date")
    xo <- as.Date(xo, origin=as.Date("1970-01-01"))
  else if(inherits(x, "POSIXt"))
    xo <- as.Date(as.POSIXct(xo, origin=as.POSIXct("1970-01-01")))
  ## Call the matrix version to create the plot
  invisible(contourPlot.matrix(zo, xo, yo, Contours=Contours,
                               yaxis.range=yaxis.range, xaxis.range=xaxis.range,
                               ylabels=ylabels, xlabels=xlabels,
                               xtitle=xtitle, ytitle=ytitle,
                               caption=caption, margin=margin))
}

#' @rdname contourPlot
#' @export
#' @method contourPlot matrix
contourPlot.matrix <- function(z, rows, cols, matrix.rows="x", # data specs
                               Contours=list(name="Auto",
                                 levels=10,
                                 filled=FALSE,
                                 lineColor="black",
                                 lineLabel="flattest",
                                 fillColors="coolWarm"), # Contour controls
                               yaxis.range=c(NA,NA), # y-axis controls
                               xaxis.range=c(NA,NA), # x-axis controls
                               ylabels=4, xlabels=4, # axis labels
                               xtitle=deparse(substitute(x)),
                               ytitle=deparse(substitute(y)), # axis titles
                               caption="",# caption
                               margin=c(NA, NA, NA, NA), ...) { # margin control
  ## Set up the matrix and axes
  xtitle <- xtitle # needed to 'set' names
  ytitle <- ytitle
  roworder <- order(rows)
  colorder <- order(cols)
  z <- z[roworder, colorder] # just in case
  if(dev.cur() == 1)
    setGD("ContourPlot")
  if(matrix.rows == "x") {
    x <- rows[roworder]
    y <- cols[colorder]
  }
  else {
    y <- rows[roworder]
    x <- cols[colorder]
    z <- t(z) # transopose to rows as x
  }
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=FALSE,
                  axis.rev=FALSE), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=FALSE,
                axis.rev=FALSE, axis.labels=ylabels)
  yax$extend.range <- FALSE
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  if(is.list(xlabels))
    xax <- c(list(data=x, axis.range=xaxis.range, axis.log=FALSE,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=x, axis.range=xaxis.range, axis.log=FALSE,
                axis.rev=FALSE, axis.labels=xlabels)
  xax$extend.range <- FALSE
  xax <- do.call("setAxis", xax)
  x <- xax$data
  xax <- xax$dax
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up the plot
  plot(range(x), range(y), type='n', xlim=xax$range, xaxs='i', axes=F,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  ## Force defaults for the coutours
  ## If filled is TRUE and lineColor is not set, then set the line color
  ## to "none"
  if(!is.null(Contours$filled) && Contours$filled &&
     is.null(Contours$lineColor))
    Contours$lineColor <- "none" # This overrides the following call
  Contours <- setDefaults(Contours, name="Auto", levels=10,
                          filled=F, lineColor="black", lineLabel="flattest",
                          fillColors="coolWarm")
  # Set line weigth automatically
  if(Contours$lineColor != "black") {
  	Contours$lineWt <- lineWt("color")
  	lineWid <- "color"
  } else {
  	Contours$lineWt <- lineWt("standard")
  	lineWid <- "standard"
  }
  ## Process levels if necessary
  if(length(Contours$levels) == 1)
    Contours$levels <- pretty(range(z, na.rm=TRUE), Contours$levels)
  ## Make a very dense grid (50 lines per inch) if filled is requested
  if(Contours$filled) {
    usr <- par("usr")
    den <- (c(usr[2] - usr[1], usr[4] - usr[3])/ par("pin")) / 72
    xrng <- range(x)
    yrng <- range(y)
    grd <- expand.grid(x=x, y=y)
    grd$z <- as.vector(z)
    grd <- na.omit(grd)
    xyz <- interp(grd$x, grd$y, grd$z, xo=seq(xrng[1], xrng[2], by=den[1]),
                  yo=seq(yrng[1], yrng[2], by=den[2])) # accept other defaults
    ## reconstruct x, y, and z
    x <- xyz$x
    y <- xyz$y
    z <- xyz$z
    ## set up for image and do it
    Colors <- get(paste(Contours$fillColors, "colors", sep='.'))
    Contours$fillColors <- Colors(length(Contours$levels) - 1)
    image(x, y, z, col=Contours$fillColors, add=TRUE,
          breaks=Contours$levels)
  } # end of filled
  ## Get contours for return
  xyz <- contourLines(x, y, z, levels=Contours$levels)
  par(family="USGS")
  if(Contours$lineColor != "none") # OK, draw lines
    contour(x, y, z, levels=Contours$levels,
            drawlabels=Contours$lineLabel != "none", labcex=0.75,
            method=ifelse(Contours$lineLabel == "none", "flattest", Contours$lineLabel),
            col=Contours$lineColor, lwd=Contours$lineWt, add=TRUE)
  ## Finish
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  ## Need explantion
  if(Contours$filled) {
    zvalues <- matrix(Contours$levels[-1L] - diff(Contours$levels)/2, nrow=1)
    fillcol <- Contours$fillColors
    breaks <- Contours$levels
    xvals <- c(.1, .35)
    yvals <- seq(to=.5, by=-.25, length.out=length(breaks)) # offset from top
    linecol <- Contours$lineColor
    name <- Contours$name
    if(name == "Auto")
    	name <- ""
    contour <- list(zvalues=zvalues, fillcol=fillcol, breaks=breaks, xvals=xvals,
                    yvals=yvals, linecol=linecol, name=name, linewt=Contours$lineWt)
    explan <- list(contour=contour)
  }
  else { # Simple explanation
    name <- if(Contours$name == "Auto") "Line of equal value" else Contours$name
    Plot <- setPlot(list(), name=name, what="lines", type="solid",
                    width=lineWid, symbol="circle", filled=TRUE,
                    size=0.09, Contours$lineColor) # force defaults if not set
    explan <- setExplan(Plot) # add info to set up explanation
  }
  invisible(list(xyz=xyz, yaxis.log=FALSE, yaxis.rev=FALSE,
                 xaxis.log=FALSE, explanation=explan, margin=margin,
                 yax=yax, xax=xax))
}
