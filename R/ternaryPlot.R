#' Ternary Diagram
#' 
#' Produce a ternary diagram, also called a trilinear or triangular diagram.
#' 
#' The \code{what} component of the \code{Plot} argument must be either
#' "points" or "none."
#' 
#' @param x the x-axis (bottom) data.
#' @param y the y-axis (left side) data.
#' @param z the z-axis (right side) data. Note that \code{x}, \code{y}, and
#' \code{z} do not need to sum to the axis range.
#' @param Plot control parameters of the plot, see \code{link{setMultiPlot}}
#' and \bold{Details} for details.
#' @param axis.range the range of the axes. Must be either c(0, 1) or c(0,
#' 100).
#' @param num.labels the number of labels to draw on each axis. Best selections
#' are 2 giving (0, 100), 3 (0, 50, 100), 5 (0, 25, 50, 75, 100), or 6 (o, 20,
#' 40, 60, 80, 100).
#' @param ticks logical, if \code{TRUE}, then draw ticks.
#' @param grids logical, if \code{TRUE}, then draw grid lines.
#' @param orient the orientation of the graph. Must be "c" for clockwise or "a"
#' for anti- or counter-clockwise
#' @param xtitle title (also called caption) for the x-axis.
#' @param ytitle title (also called caption) for the y-axis.
#' @param ztitle title (also called caption) for the z-axis.
#' @param units.title the units titles, should be either "Percent" of
#' "Proportion" depending on \code{axis.range}.
#' @param caption the figure caption.
#' @param margin set up the plot area margins--- ignored, included for
#' consistency with other plotting functions in this package.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{piperPlot}.
#' @seealso \code{\link{setPage}}, \code{link{setMultiPlot}},
#' \code{\link{piperPlot}}, \code{\link{addTernary}}
#' @references Lorenz D.L., 2015, smwrGraphs
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of ternaryPlot:
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' }
#' @export ternaryPlot
ternaryPlot <- function(x, y, z, # data to plot (this will sum to range)
                        Plot=list(name="", what="points", type="solid",
                           width="standard", symbol="circle", filled=TRUE,
                           size=0.09, color="black"), # plot controls
                        ## plot controls (for each point)
                        axis.range=c(0,100), num.labels=6, ticks=TRUE,
                        grids=!ticks, orient="c", # axis controls and labels
                        xtitle=deparse(substitute(x)),
                        ytitle=deparse(substitute(y)),
                        ztitle=deparse(substitute(z)), # axis titles
                        units.title="Percent", # axis titles
                        caption="", margin=c(NA, NA, NA, NA)) {
	# Coding history:
	#    2012Nov02 DLLorenz Start revisions for pub-quality output
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  ## Begin Execution, normalize the data according to range
  xtitle <- xtitle
  ytitle <- ytitle
  ztitle <- ztitle
  units.max <- axis.range[2]
  tsum <- sumComposition(x, y, z, Range=units.max)
  Data <- as.data.frame(tsum)
  x <- tsum[,1]
  y <- tsum[,2]
  z <- tsum[,3]
  ## Set up the main plot scalled to correct aspect ratio
  if(dev.cur() == 1)
    setGD("TrilinearPlot")
  plot(axis.range, axis.range*sqrt(3)/2,axes=FALSE,type="n", asp=1,
       xlab=units.title,ylab="")
  ## Process plot control
  what=Plot$what[1L]
  parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black")
  symbol <- parms$current$pch
  color <- parms$current$col
  size <- parms$current$csi
  par(xpd=TRUE)
  orient <- paste(orient, "*", sep="")
  ternarySubplot(x, y, z, what, symbol, color, size,
                 axis.range=axis.range, num.labels=num.labels,
                 ticks=ticks, grids=grids, orient=orient,
                 xtitle=xtitle, ytitle=ytitle,
                 ztitle=ztitle)
  Data <- ternarySubplot(x, y, z, axis.range=axis.range,
                         orient=orient, plot=FALSE)
  ## Caption if necessary
  if(caption != "")
    addCaption(caption)
  ## Return
  retval <- list(x=Data$x, y=Data$y, Data=tsum, orient=orient,
                 axis.range=axis.range, explanation=parms$Explan)
  invisible(retval)
}
