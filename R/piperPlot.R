#' Piper Diagram
#' 
#' Produces a Piper diagram.
#' 
#' @details The \code{what} component of the \code{Plot} argument must be either
#' "points" or "none."
#' 
#' The units for \code{xCat}, \code{yCat}, \code{zCat}, \code{xAn}, 
#'\code{yAn}, and \code{zAn} should be in milli-equivalents.
#' 
#' @param xCat data for the cation x-axis, generally calcium.
#' @param yCat data for the cation y-axis, generally magnesium.
#' @param zCat data for the cation z-axis, generally sodium plus potasium.
#' @param xAn data for the anion x-axis, generally chloride plus other minor
#' constituents.
#' @param yAn data for the anion y-axis, generally carbonate plus bicarbonate.
#' @param zAn data for the anion z-axis, generally sulfate.
#' @param Plot control parameters of the plot, see \code{link{setMultiPlot}}
#' and \bold{Details} for details.
#' @param axis.range the range of the axes. Must be either c(0, 1) or c(0,
#' 100).
#' @param num.labels the number of labels to draw on each axis. Best selections
#' are 2 giving (0, 100), 3 (0, 50, 100), 5 (0, 25, 50, 75, 100), or 6 (o, 20,
#' 40, 60, 80, 100).
#' @param ticks logical, if \code{TRUE}, then draw ticks.
#' @param grids logical, if \code{TRUE}, then draw grid lines.
#' @param xCat.title title (also called caption) for the cation x-axis.
#' @param yCat.title title (also called caption) for the cation y-axis.
#' @param zCat.title title (also called caption) for the cation x-axis.
#' @param xAn.title title (also called caption) for the anion x-axis.
#' @param yAn.title title (also called caption) for the anion y-axis.
#' @param zAn.title title (also called caption) for the anion z-axis.
#' @param x.yCat.title title for the cation x- and y-axis for the central Piper
#' graph.
#' @param x.zAn.title title for the anion x- and z-axis for the central Piper
#' graph.
#' @param units.title the units titles, should be either "Percent" of
#' "Proportion" depending on \code{axis.range}.
#' @param caption the figure caption.
#' @param margin set up the plot area margins---ignored, included for
#' consistency with other plotting functions in this package.
#' @return Information about the graph and current plot.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{piperPlot}.
#' 
#' @seealso \code{\link{setPage}}, \code{\link{setMultiPlot}},
#' \code{\link{ternaryPlot}}, \code{\link{addPiper}}
#' @references Hem J.D., 1989, Study and interpretation of the chemical
#' characteristics of natural water: U.S. Geological Survey Water-Supply Paper
#' 2254, 263 p.
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of piperPlot:
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' demo(topic="PiperScript", package="smwrGraphs")
#' }
#' @export piperPlot
piperPlot <- function(xCat, yCat, zCat, xAn, yAn, zAn, # data (need not sum to 100)
                      Plot=list(name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black"), # plot controls (for each point)
                      axis.range=c(0,100), num.labels=6, ticks=FALSE, 
                      grids=!ticks, # axis controls and labels
                      xCat.title="Calcium", yCat.title="Magnesium",
                      zCat.title="Sodium plus Potassium",
                      xAn.title="Chloride, Fluoride, Nitrite plus Nitrate",
                      yAn.title="Carbonate plus Bicarbonate",
                      zAn.title="Sulfate",
                      x.yCat.title="Calcium plus Magnesium",
                      x.zAn.title="Sulfate plus Chloride", 
                      units.title="Percent", # axis titles
                      caption="", # caption 
                      margin=c(NA, NA, NA, NA)) {# margin controls
	# Coding history:
	#    2001Mar02 DLLorenz Original coding.
	#    2002Apr05 DLLorenz Add explanation.
	#    2002Jul24 DLLorenz Revise color and symbol argument usage and error trapping.
	#    2004Apr30 DLLorenz Fixed units bug in piperplot.
	#    2008Jul08 DLLorenz Start revisions for pub-quality output
	#    2011Jan22 DLLorenz Conversion to R
	#    2011Apr07 DLLorenz First working version in R
	#    2011Apr16 DLLorenz Added complete complement of args to setMultiPlot
	#    2011May24 DLLorenz Argument documentation
	#    2011Oct19 DLLorenz Fix logical abbrevs and others
	#    2012Feb18 DLLorenz Fixed for specified figure instead of centered
	#    2012Nov02 DLLorenz Tweak ternarySub for centered axis titles 
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  ## Simple sub region set up function:
  setSubRgn <- function(xll, yll, xur, yur, fig, curmai, usr, pin) {
    ## Set marigns according to the user coordinates in the call
    ## Compute the desired plotting region by setting up new margins
    uin <- pin/c(usr[2] - usr[1], usr[4] - usr[3]) # inches per user unit, x and y
    newmai <- c(curmai[1] + (yll - usr[3]) * uin[2], curmai[2] + (xll - usr[1]) *
                uin[1], curmai[3] + (usr[4] - yur) * uin[2], curmai[4] +
                (usr[2] - xur) * uin[1])
    par(fig=fig, mai = newmai, xpd=TRUE)
    invisible(par(no.readonly=TRUE))
  } # End of setSubRgn function
  ## Begin Execution, normalize the data according to range
  ##   and revert to old names
  units.max <- axis.range[2]
  tsum <- sumComposition(xCat, yCat, zCat, Range=units.max)
  cations <- as.data.frame(tsum) # part of returned data
  names(cations) <- make.names(c(xCat.title, yCat.title, zCat.title))
  ca <- tsum[,1]
  mg <- tsum[,2]
  na.k <- tsum[,3]
  tsum <- sumComposition(xAn, yAn, zAn, Range=units.max)
  anions <- as.data.frame(tsum) # part of returned data
  names(anions) <- make.names(c(xAn.title, yAn.title, zAn.title))
  cl.f.no2.no3 <- tsum[,1]
  co3.hco3 <- tsum[,2]
  so4 <- tsum[,3]
  ## Set up the main plotting axis as square, ranging from -1 to 1 on both axes.
  ## This is manual set up for square plot--larger than the default
  if(dev.cur() == 1)
    setGD("PiperPlot")
  fin <- par("fin")
  vdist <- fin[2] - 1.24
  hdist <- fin[1] - 0.44
  if(vdist > hdist)
    mai <- c(1.02 + (vdist - hdist), .22, .22, .22)
  else
    mai <- c(1.02, .22 + (hdist - vdist)/2, .22, .22 + (hdist - vdist)/2)
  par(xaxs="i", yaxs="i", mai=mai, family="USGS")
  fig <- par("fig")
  plot(c(-1,1),c(-1,1),axes=FALSE,type="n",xlab=units.title,ylab="")
  ## Process plot control
  what=Plot$what[1]
  parms <- setMultiPlot(Plot, length(ca), name="", what='points', type='solid',
                        width='standard', symbol='circle', filled=TRUE,
                        size=0.09, color='black')
  symbol <- parms$current$pch
  color <- parms$current$col
  size <- parms$current$csi
  plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE) # part of returned data
  adjSize <- par('csi') # This just happens to work out nicely!
  adjHei <- (1 - adjSize) * sqrt(3) / 2
  ## Plot the ternary diagrams and get the x-y data
  oldpars <- par(no.readonly=TRUE)
  ## Set up subregion for cation plot
  catplot <- setSubRgn(-1, -1, -adjSize, adjHei - 1, fig, mai,
                       oldpars$usr, oldpars$pin)
  catplot$usr <- ternarySubplot(ca, mg, na.k, what, symbol, color, size,
                                axis.range=axis.range, num.labels=num.labels,
                                ticks=ticks,grids=grids, 
                                xtitle=xCat.title, ytitle=yCat.title,
                                ztitle=zCat.title)
  cations <- cbind(cations, ternarySubplot(ca, mg, na.k, axis.range=axis.range,
                                           plot=FALSE))
  par(oldpars) # Reset to original
  anplot <- setSubRgn(adjSize,-1, 1, adjHei - 1, fig, mai,
                      oldpars$usr, oldpars$pin)
  anplot$usr <- ternarySubplot(cl.f.no2.no3, co3.hco3, so4, what, symbol,
                               color, size, axis.range=axis.range,
                               num.labels=num.labels, ticks=ticks, 
                               grids=grids, orient="a",
                               xtitle=xAn.title,ytitle=yAn.title, ztitle=zAn.title)
  anions <- cbind(anions, ternarySubplot(cl.f.no2.no3, co3.hco3, so4,
                                         axis.range=axis.range, orient="a",
                                         plot=FALSE))
  ## Plot the piper diagram
  y <- so4 + cl.f.no2.no3
  adjBot <- adjSize*sqrt(3) - 1
  adjWid <- (.732 - adjBot)/2/sqrt(3)
  par(oldpars)
  piperplot <- setSubRgn(-adjWid, adjBot, adjWid, .732, fig, mai,
                         oldpars$usr, oldpars$pin)
  piperplot$usr <- piperSubplot(na.k, y, what, symbol, color, size,
                                axis.range=axis.range, num.labels=num.labels,
                                ticks=ticks, grids=grids, x1title="",
                                y1title=x.zAn.title, x2title=x.yCat.title,
                                y2title="")
  piper <- as.data.frame(piperSubplot(na.k, y, axis.range=axis.range,
                                      plot=FALSE))
  ## Finish the axis labels and caption
  par(oldpars)
  par(fig=fig)
  toff <- 0.5 + par("csi")
  text(x=-toff, y=0.0, labels=units.title, srt=60, family="USGS")
  text(x=toff, y=0.0, labels=units.title, srt=300, family="USGS")
  if(caption != "")
    mtext(text=caption, side=1, line=toff - 0.5, 
          at=par("usr")[1], adj=0, family="USGS")
  ## Return
  retval <- list(cations=cations, anions=anions, piper=piper, plotInfo=plot.info,
                 axis.range=axis.range, catplot=catplot, anplot=anplot,
                 piperplot=piperplot, explanation=parms$Explan)
  invisible(retval)
}
