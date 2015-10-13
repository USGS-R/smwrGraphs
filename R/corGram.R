#' Correlogram
#' 
#' Create a correlogram for irregularly spaced data
#' 
#' @details \code{CorGram} is a list with these components: 
#' \describe{ 
#' \item{band}{a measure of the bandwidth used by \code{\link[KernSmooth]{locpoly}}}
#' \item{kernel}{the kernel used, currently ignored}
#' \item{color}{the color of the line representing the correlogram}
#' \item{width}{the line width of the line representing the correlogram}
#' \item{add0line}{logical, if \code{TRUE}, then add the 0 line; if \code{FALSE}, 
#' then do not draw the 0 line}}
#' 
#' @param x decimal time.
#' @param y residuals or other observations, these will be scaled, but not
#' centered.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param CorGram control parameters of the correlogram line. See \bold{Details}.
#' @param yaxis.range set the y-axis range.
#' @param xaxis.range set the x-axis range.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set the x-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @return Information about the graph.
#' @importFrom KernSmooth locpoly
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{corGram}.
#' @seealso \code{\link{setPage}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of corGram:
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export corGram
corGram <- function(x, y, # data specs
                    Plot=list(name="Standardized Observations",
                      what='points', type='solid',
                      width='standard', symbol='circle', filled=TRUE,
                      size=0.03, color='gray40'), # plot controls
                    CorGram=list(band=0.15, kernel='normal',
                      color='black', width='standard',
                      add0line=TRUE), # correlogram controls
                    yaxis.range=c(-3,3), # y-axis controls
                    xaxis.range=c(0,1.5), # x-axis controls
                    ylabels=7, xlabels=4, # labels
                    xtitle="Difference in Time, in Years",
                    ytitle="Standardized Serial Correlation", # axis titles
                    caption="",# caption
                    margin=c(NA, NA, NA, NA))  { # margin control
	# Coding history:
	#    2006Jun06 DLLorenz Initial coding
	#    2008Nov10 DLLorenz Vectorized version
	#    2009Mar23 DLLorenz Begin editorial and method changes
	#    2009Mar27 DLLorenz Moved to Graphs
	#    2010Nov29 DLLorenz Modified for R
	#    2011Apr16 DLLorenz Added complete complement of args to setPlot
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Aug17 DLLorenz Changes to default Plot, titles, and corGram
	#    2011Oct03 DLLorenz changed ksmooth to locpoly
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Mar23 DLLorenz More tweaks for kernel smoothing
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  ## Initial processing
  xx <- outer(x, x, "-")
  y <- as.vector(scale(y))
  yy <- outer(y, y)
  if(dev.cur() == 1)
    setGD("Correlogram")
  ## set up the axes
  yax <- setAxis(yy, yaxis.range, FALSE, FALSE, ylabels)
  yax <- yax$dax
  xax <- setAxis(xx, xaxis.range, FALSE, FALSE, xlabels)
  xax <- xax$dax
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Plot the points, need to suppress graphics warnings
  plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  Plot <- setPlot(Plot, name="Standardized Observations",
                  what='points', type='solid',
                  width='standard', symbol='dot', filled=TRUE,
                  size=0.09, color='gray') # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  points(xx[xx>0], yy[xx>0], type=plotPars$type, lwd=plotPars$lwd,
         lty=plotPars$lty, pch=plotPars$pch, cex=plotPars$cex,
         col=plotPars$col, bg=plotPars$col)
  ## Plot the correlogram
  CorGram <- setDefaults(CorGram, band=0.15, kernel='normal',
                      color='black', width='standard', add0line=TRUE)
  xpoints <- c(-.25, par('usr')[2] + 0.1) # Seems to need a buffer
  ksmo <- locpoly(xx[xx !=0], yy[xx!=0], degree=2,
                              kernel = CorGram$kernel,
                              bandwidth = CorGram$band, range.x=xpoints,
                              truncate=FALSE, gridsize=121)
  Plot$name <- 'Empirical Correlogram'
  Plot$what <- 'lines'
  Plot$color <- CorGram$color
  Plot$width <- CorGram$width
  explan <- setExplan(Plot, explan) # add info to set up explanation
  plotPars <- explan$current  
  lines(ksmo, type=plotPars$type, lwd=plotPars$lwd,
         lty=plotPars$lty, pch=plotPars$pch, cex=plotPars$cex,
         col=plotPars$col, bg=plotPars$col)
  if(CorGram$add0line)
    refLine(horizontal=0.0, Plot=list(what='lines', type='dashed',
                       width=CorGram$width, color=CorGram$color))
  par(lwd=frameWt()) # frame line width
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(list(x = xx[xx>0], y = yy[xx>0], smooth = ksmo, yaxis.log=FALSE,
                 yaxis.rev=FALSE, xaxis.log=FALSE, explanation=explan,
                 margin=margin, yax=yax, xax=xax))
}
