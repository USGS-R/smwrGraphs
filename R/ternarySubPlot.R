#' Piper Diagram
#' 
#' Produce a trilinear diagram for the Piper Diagram (support function).
#' 
#' Support function, to be called only from \code{piperPlot}.
#' 
#' @param x the x-ccordinate values.
#' @param y the y-coordinate values.
#' @param z the z-coordinate values.
#' @param what the type of plot, must be either "points," "lines," or "none."
#' @param symbol the symbol to use if \code{what} is "points."
#' @param color the color of the plot.
#' @param size the size of the symbol if \code{what} is "points."
#' @param axis.range the range of the axes. Must be either c(0, 1) or c(0,
#' 100).
#' @param num.labels the number of labels to draw on each axis.
#' @param ticks logical, if \code{TRUE}, then draw ticks.
#' @param grids logical, if \code{TRUE}, then draw grid lines.
#' @param orient a single character, "c" indicates clockwise orientation for
#' \code{x}, \code{y}, and \code{z} anything else indicates counter-clockwise.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param ztitle the z-axis title (also called z-axis caption).
#' @param plot plot the data?
#' @return If \code{plot} is TRUE, then the range of the user coordinates.
#' Otherwise, the \code{x}, \code{y}, and \code{z} values are converted to
#' 2-dimensional values.
#' @seealso \code{\link{piperPlot}}
#' @keywords aplot
#' @export ternarySubplot
ternarySubplot <- function(x, y, z, # data to plot (must sum to range)
													 what="points",
													 symbol = rep(1, length(x)), color=rep(1, length(x)),
													 size = rep(0.05, length(x)),
													 ## plot controls (for each point)
													 axis.range=c(0,100), num.labels=6, ticks=FALSE,
													 grids=!ticks, orient="c", # axis controls and labels
													 xtitle="x", ytitle="y", ztitle="z", #axis titles
													 plot=TRUE) { # plot or just return data
	##
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
}