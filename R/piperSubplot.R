#' Piper Diagram
#' 
#' Plot the Piper Diagram projected from the two trilinear diagrams on either
#' side (support function).
#' 
#' Support function, to be called only from \code{piperPlot}.
#' 
#' @param x x-axis coordinate values (derived from the cation z-axis).
#' @param y y-axis coordinate values (derived from the anion y-axis).
#' @param what the type of plot, must be either "points" or "lines."
#' @param symbol the symbol to use if \code{what} is "points."
#' @param color the color of the plot.
#' @param size the size of the symbol if \code{what} is "points."
#' @param axis.range the range of the axes. Must be either c(0, 1) or c(0,
#' 100).
#' @param num.labels the number of labels to draw on each axis.
#' @param ticks draw ticks?
#' @param grids draw grid lines?
#' @param x1title the title (also called caption) of the bottom x-axis.
#' @param y1title the title (also called caption) of the left y-axis.
#' @param x2title the title (also called caption) of the top x-axis.
#' @param y2title the title (also called caption) of the right y-axis.
#' @param plot plot the data?
#' @return If \code{plot} is TRUE, then the range of the user coordinates.
#' Otherwise, the transformed x- and y-coordinate values.
#' @seealso \code{\link{piperPlot}}
#' @keywords aplot
#' @export piperSubplot
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
