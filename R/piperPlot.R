# Creates a Piper diagram.
#    Calls ternarySubplot and piperSubplot.
#
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
#

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
  ## Arguments:
  ##  xCat (numeric vector) and
  ##  yCat (numeric vector) and
  ##  zCat (numeric vector) the cations for the x-, y-, and z-axes. Need not sum to
  ##    1 or 100.
  ##  xAn (numeric vector) and
  ##  yAn (numeric vector) and
  ##  zAn (numeric vector) the anions for the x-, y-, and z-axes. Need not sum to
  ##    1 or 100.
  ##  Plot (list) defining the characteristics of the plot
  ##  axis.range (2 element numeric vector) the range of the axes. Must be either
  ##    c(0, 1) or c(0, 100).
  ##  num.labels (numeric scalar) the number of labels to draw on each axis. Best
  ##    if one of 2 giving (0, 100), 3 (0, 50, 100), 5 (0, 25, 50, 75, 100), or
  ##    6 (o, 20, 40, 60, 80, 100).
  ##  ticks (logical scalar) indicator to draw ticks.
  ##  grids (logical scalar) indicator to draw grid lines.
  ##  xCat.title (character scalar) and
  ##  yCat.title (character scalar) and
  ##  zCat.title (character scalar) the axis titles for the x-, y-, and z-axes
  ##    for the cations.
  ##  xAn.title (character scalar) and
  ##  yAn.title (character scalar) and
  ##  zAn.title (character scalar) the axis titles for the x-, y-, and z-axes
  ##    for the anions.
  ##  x.yCat.title (character scalar) and
  ##  x.zAn.title (character scalar) the axis titles for the respective axes on
  ##    the central plot
  ##  units.title (character scalar) the units titles, should be either "Percent"
  ##    of "Proportion" depending on axis.range
  ##  caption (character scalar) the figure caption
  ##  margin (4 element numeric vector) the margin for the labels and titles,
  ##    ignored, included for consistency with other plotting functions in this
  ##    package.
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

## Subplots follow
## Create a ternary sub plot for the Piper Diagram
ternarySubplot <- function(x, y, z, # data to plot (must sum to range)
                           what="points",
                           symbol = rep(1, length(x)), color=rep(1, length(x)),
                           size = rep(0.05, length(x)),
                           ## plot controls (for each point)
                           axis.range=c(0,100), num.labels=6, ticks=FALSE,
                           grids=!ticks, orient="c", # axis controls and labels
                           xtitle="x", ytitle="y", ztitle="z", #axis titles
                           plot=TRUE) { # plot or just return data
  ## Arguments:
  ##  x, y, z (numeric) the data to plot
  ##  what (character) "points" or "none", anythong other than points = none
  ##  symbol (numeric or character) the symbol for points
  ##  color  (numeric or character) the color for each point
  ##  size (numeric) the size of the points or the line width
  ##  axis.range (numeric vector of length 2) the range of the axis, either
  ##   c(0, 1) or c(0, 100). Others are possible but why?
  ##  num.labels (numeric) the number of labels, should be one of 2, 3, 5, 6, 11
  ##  ticks, grids (logical) draw ticks or grids?
  ##  xtitle, ytitle, ztitle, (character) the axis titles
  ##  plot (logical) draw the plot, or just compute the transformed coordinates?
  ##
  ## Draw a ternary plot of three compositional variables x,y,z.
  ## Produces an equilateral triangle with each side running from 
  ##     min to max. Points are placed within the triangle to 
  ##     indicate how much of each component is present at that point.
  ## Axes are oriented "clockwise" or "anticlockwise."
  ##  Note that "c*" and "a*" are tweaks for correct title placement for
  ##  the stand-alone ternary plot.
  ## Compute midpoint of x and y/z axes
  min <- axis.range[1]
  max <- axis.range[2]
  plotsize <- par("pin")
  ticklen <- .08/min(plotsize) # ticks are 0.08 inches
  jitter.tick <- ticklen*max # convert to user units
  mid <- (max+min)/2
  height <- sqrt(max^2 - mid^2)
  qmid<- mid/2
  qheight <- sqrt(mid^2 - qmid^2)
  ## Plot axes if requested
  ## Preliminary computations
  ii <- num.labels + 1 # needed for some grids
  sqrt3 <- sqrt(3) # compute it once here
  if(plot) {
    par(xaxs="i",yaxs="i",pty="s", new=TRUE)
    plot(c(min,mid,max), c(min,height,min), type="n", axes=FALSE,
         xlab="",ylab="")
    par(lwd=frameWt()) # frame line width
    lines(c(min,max,mid,min), c(min,min,height,min), type="l")
    jitter.label <- par("cxy")[1]*1.1
    
    ## Set up labels on three axes
    x.along <- seq(from=min, to=max, length=num.labels)
    y.along <- seq(from=min, to=height, length=num.labels)
    z.along <- seq(from=mid, to=max, length=num.labels)
    if (substring(orient, 1, 1) == "c") {
      axis.labels <- paste(" ", format(rev(x.along)), " ")
      xadj <- 0
      xsrt <- -60
      ysrt <- 0
      zsrt <- 60
    }
    else {
      axis.labels <- paste(" ", format(x.along), " ")
      xadj <- 1
      xsrt <- 60
      ysrt <- 300
      zsrt <- 0
    }
    ## Lower (x) axis labels, ticks, grid, and title
    text(x=x.along, y=rep(min, num.labels), labels=axis.labels,
         adj=xadj, srt=xsrt, family='USGS')
    if(ticks)
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i]-jitter.tick/3,x.along[i],x.along[i]+jitter.tick/3)
              ,c(min+jitter.tick*sqrt3/3,min,min+jitter.tick*sqrt3/3))
    if(grids)
      for(i in seq(from=2,length=num.labels-2))
        segments(x.along[i], 0, x.along[i]-y.along[i]/sqrt3, y.along[i])
    text(x=mid, y=-2.55 * jitter.label, labels=xtitle, family='USGS') # approximate factor
    ## Left (y) axis labels, ticks, grid and title
    text(x=x.along/2, y=y.along, labels=rev(axis.labels), adj=1, srt=ysrt, family='USGS')
    if(ticks)
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i]+jitter.tick*4/3, x.along[i], x.along[i]+jitter.tick*2/3)/2,
              c(y.along[i], y.along[i], y.along[i] - jitter.tick*sqrt3/3))
    if(grids)
      for(i in seq(from=2,length=num.labels-2))
        segments(z.along[ii - i], y.along[i], max - z.along[ii - i], y.along[i])
    if(orient == "a")
      text(x=qmid-jitter.label*1.5*sqrt(16/3)/2,
           y=qheight+1.5*jitter.label*sqrt(16/3), labels=ytitle, srt=60, family='USGS')
    else
      text(x=qmid-jitter.label*1.5*sqrt3, y=qheight+1.5*jitter.label,
           labels=ytitle, srt=60, family='USGS')
    ## Right (z) axis labels, ticks, grid and title
    text(x=z.along, y=rev(y.along), labels=rev(axis.labels),adj=0, srt=zsrt, family='USGS')
    if(ticks)
      for(i in seq(from=2,length=num.labels-2))
        lines(c(z.along[i]-jitter.tick*2/3, z.along[i], z.along[i]-jitter.tick/3),
              c(y.along[num.labels+1-i], y.along[num.labels+1-i], y.along[num.labels+1-i] - jitter.tick*sqrt3/3))
    if(grids)
      for(i in seq(from=2,length=num.labels-2))
        segments(z.along[ii - i], y.along[i], x.along[ii - i], 0)
    if(orient == "c")
      text(x=3*qmid+jitter.label*1.5*sqrt(16/3)/2,
           y=qheight+1.5*jitter.label*sqrt(16/3), labels=ztitle, srt=300, family='USGS')
    else
      text(x=3*qmid+jitter.label*1.5*sqrt3, y=qheight+1.5*jitter.label,
           labels=ztitle, srt=300, family='USGS')
  } # end of if(plot)
  ## Transform the position of the points
  if (substring(orient, 1, 1) == "c") {
    x.triangle <- (z+max-x)/2
    y.triangle <- sqrt3/2*y
  }
  else {
    x.triangle <- (x+max-y)/2
    y.triangle <- sqrt3/2*z
  }
  if(!plot) # just return the data, not called as subplot
    return(list(x=x.triangle, y=y.triangle))
  
  ## plot the data
  par(lwd=stdWt()) # use default line weight
  what <- match.arg(what, c("points", "none"))
  if(what == "points") {
    size <- size/par("csi") # convert to cex parameter
    for(i in which(!is.na(x.triangle + y.triangle)))
      points(x.triangle[i], y.triangle[i], pch=symbol[i], col=color[i],
             cex=size[i], bg=color[i])
  }
  ## return usr if plotted
  return(c(min, max, min, height))
} # End of ternarySubPlot

## Create the piper sub plot for the Piper Diagram
piperSubplot <- function(x, y, # data to plot only x and y are needed
                         what='points',
                         symbol = rep(1, length(x)), color=rep(1, length(x)),
                         size = rep(0.05, length(x)),
                         ## plot controls (for each point)
                         axis.range=c(0,100), num.labels=6, ticks=FALSE,
                         grids=!ticks, # axis controls and labels
                         x1title="x1", y1title="y1", x2title="x2",
                         y2title="y2", # axis titles
                         plot=TRUE) { # plot or just return data
  ## Draw a piper plot of four compositional variables x1(x), y1(y) ,x2,
  ## and y2.  The columns x2 and y2 are not needed because x1+x2=max and y1+y2=max.
  ## Produces a rhombus to match the triangles of a ternary plot.
  ## Points are placed within the square to indicate how much of each component
  ## is present at that point.
  ## This routine is intended to be called as a subplot.  It must be called
  ## with a plotting region exactly 2 times higher than wide.
  ## Compute initial variables
  min <- axis.range[1]
  max <- axis.range[2]
  plotsize <- par("pin")
  ticklen <- .08/min(plotsize) # ticks are 0.08 inches
  jitter.tick <- ticklen*max # convert to user units
  jitter.grid <- 3*max/2
  sqrt3 <- sqrt(3) # compute it once here
  orient.mat <- matrix(c(.5,.5,-sqrt(3)/2,sqrt(3)/2),nrow=2)
  mid <- (max+min)/2
  height <- sqrt(max^2 - mid^2)
  qmid<- mid/2
  qheight <- sqrt(mid^2 - qmid^2)
  ## Plot axes is requested
  if(plot) {
    par(xaxs="i",yaxs="i", new=TRUE)
    plot(c(min,height), c(-height,height), type="n", axes=FALSE,
         xlab="",ylab="",
         xlim=c(min,max),ylim=c(-height,height))
    par(lwd=frameWt()) # frame line width
    jitter.label <- par("cxy")[1]*1.1
    ## Set up labels.
    labels.along <- seq(from=min, to=max, length=num.labels)
    axis.labels <- paste(" ", format(labels.along), " ")
    ## Lower left axis (x1)
    ## Draw axis line
    x.along <- labels.along/2
    y.along <- -labels.along*sqrt(3)/2
    lines(x.along,y.along)
    ## Add axis title
    text(x=qmid-jitter.label*1.5*sqrt(3), y=-qheight-1.5*jitter.label, labels=x1title,
         srt=-60, family='USGS')
    ## Add labels
    text(x=x.along, y=y.along, labels=axis.labels, adj=1, srt=60, family='USGS')
    ## Ticks
    if(ticks) {
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i],x.along[i]+jitter.tick/3)
              ,c(y.along[i],y.along[i]+jitter.tick/sqrt3),type="l")
    }
    ## Grid lines
    if(grids) {
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i], x.along[i]+jitter.grid/3),
              c(y.along[i], y.along[i]+jitter.grid/sqrt3), type="l")
    }
    ## Upper left axis (y1)
    ## Draw axis line
    y.along <- labels.along*sqrt(3)/2
    lines(x.along,y.along)
    ## Add axis title
    text(x=qmid-jitter.label*1.5*sqrt(3), y=qheight+1.5*jitter.label,
         labels=y1title, srt=60, family='USGS')
    ## Add labels
    text(x=x.along, y=y.along, labels=axis.labels, adj=1, srt=300, family='USGS')
    ## Ticks
    if(ticks) {
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i], x.along[i]+jitter.tick/3),
              c(y.along[i], y.along[i]-jitter.tick/sqrt3), type="l")
    }
    ## Grid lines
    if(grids) {
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i], x.along[i]+jitter.grid/3),
              c(y.along[i], y.along[i]-jitter.grid/sqrt3), type="l")
    }
    ## Upper right axis (x2)
    ## Draw axis line
    x.along <- labels.along/2 + mid
    y.along <- rev(labels.along*sqrt(3)/2)
    lines(x.along,y.along)
    ## Add axis title
    text(x=3*qmid+jitter.label*1.5*sqrt(3), y=qheight+1.5*jitter.label,
         labels=x2title, srt=300, family='USGS')
    ## Add labels
    text(x=x.along, y=y.along, labels=rev(axis.labels), adj=0, srt=60, family='USGS')
    ## Ticks
    if(ticks) {
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i],x.along[i]-jitter.tick/3),
              c(y.along[i],y.along[i]-jitter.tick/sqrt3),type="l")
    }
    ## Lower right axis (y2)
    ## Draw axis line
    x.along <- labels.along/2 + mid
    y.along <- -rev(labels.along*sqrt(3)/2)
    lines(x.along,y.along)
    ## Add axis title
    text(x=3*qmid+jitter.label*1.5*sqrt(3), y=-qheight-1.5*jitter.label,
         labels=y2title, srt=60, family='USGS')
    ## Add labels
    text(x=x.along, y=y.along, labels=rev(axis.labels),adj=0, srt=-60, family='USGS')
    ## Ticks
    if(ticks) {
      for(i in seq(from=2,length=num.labels-2))
        lines(c(x.along[i], x.along[i]-jitter.tick/3),
              c(y.along[i], y.along[i]+jitter.tick/sqrt3), type="l")
    }
    ## Position points
    xy.mat <- cbind(x,y) %*% orient.mat
    x.triangle <- xy.mat[,1]
    y.triangle <- xy.mat[,2]
    ## Plot points
    par(lwd=stdWt()) # use default line weight
    what <- match.arg(what, c("points", "none"))
    if(what == "points") {
      size <- size/par("csi") # convert to cex parameter
      for(i in which(!is.na(x.triangle + y.triangle)))
        points(x.triangle[i], y.triangle[i], pch=symbol[i], col=color[i],
               cex=size[i], bg=color[i])
    }
  } # End of if plot
  else { # Just return modified data, not called as subplot
    xy.mat <- cbind(x,y) %*% orient.mat
    x.triangle <- xy.mat[,1]
    y.triangle <- xy.mat[,2]
    return(list(x=x.triangle, y=y.triangle))
  }
  ## Return usr if plotted
  return(c(min, max, -height, height))
}
