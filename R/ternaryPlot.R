# create a ternary (trilinear, triangular) diagram
#
# Coding history:
#    2012Nov02 DLLorenz Start revisions for pub-quality output
#    2013Apr09 DLLorenz Added setGD 
#

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
  ## Draw a ternary plot of three compositional variables x,y,z.
  ## Produces an equilateral triangle with each side running from 
  ##     min to max. Points are placed within the triangle to 
  ##     indicate how much of each component is present at that point.
  ## Axes are oriented "clockwise" or "anticlockwise."
  ## Must be called with a square plot area.
  ## Compute midpoint of x and y/z axes
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
