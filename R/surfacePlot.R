# Surface plot
#
# Coding history:
#    2014Jun09 DLLorenz Original Coding
#
###surfacePlot 
surfacePlot <- function(pre, z.color="lightblue", # The data
							 Surface=list(name="", lineColor="black",
							 						 levels=20, ramp="coolWarm"),
							 xtitle="", ytitle="", ztitle="",
							 margin = c(NA, NA, NA, NA), caption="") { # Axis set up
	# Extract some from pre
	x <- pre$x
	y <- pre$y
	z.surf <- pre$z
	# Process z.color
	NXm1 <- length(x) - 1L
	NYm1 <- length(y) - 1L
	if(is.character(z.color)) {
		z.color <- matrix(z.color, nrow=NXm1, ncol=NYm1)
	} else if(length(x) == nrow(z.surf) && length(y) == ncol(z.surf)) {
		# Resample to 1 less that ncol and nrow
		z.tmp <- z.color
		z.color <- matrix(0, nrow=NXm1, ncol=NYm1)
		for(i in seq(NXm1))
			for(j in seq(NYm1))
				z.color[i,j] <- (z.tmp[i,j] + z.tmp[i+1L,j] + z.tmp[i,j+1L] + 
													z.tmp[i+1L,j+1L])/4
	}
	# Check
	if(NXm1 != nrow(z.color))
		stop("the length of x must 1 greater than the number of rows in z.color")
	if(NYm1 != ncol(z.color))
		stop("the length of y must 1 greater than the number of columns in z.color")
	# Process the colors
	Surface <- setDefaults(Surface, name="", lineColor="black",
												 levels=20, ramp="coolWarm")
	if(is.numeric(z.color)) { # set colors
		if(!(Surface$ramp %in% c("gray", "grey"))) # the gray scale has a different name
			ColorRamp <- get(paste(Surface$ramp, 'colors', sep='.')) # The function
		else
			ColorRamp <- gray
		if(length(Surface$levels) == 1L) {
			ColorBrk <- pretty(range(z.color, na.rm=TRUE), Surface$levels)
		} else
			ColorBrk <- Surface$levels
		color <- cut(z.color, breaks=ColorBrk, include.lowest=TRUE)
		ColorNdx <- rev(levels(color))
		Surface$levels <- length(ColorNdx) # Reset the number of levels
		## Assign colors to name
		ColorVals <- ColorRamp(Surface$levels)
		names(ColorVals) <- levels(color)
		z.color <- matrix(ColorVals[color], ncol=NYm1, nrow=NXm1)
		contour <- list(zvalues=matrix(ColorBrk[-1L] - diff(ColorBrk)/2, nrow=1),
										fillcol=ColorVals,
										breaks=ColorBrk,
										xvals=c(0.10, 0.35),
										yvals=seq(to=.5, by=-.25, length.out=length(ColorBrk)),
										linecol=Surface$lineColor,
										name=Surface$name)
	}
	# Set up the plotting parameters
	plot.new()
	margin <- na2miss(margin, 0)
	par(mar=margin)
	fin <- par("pin")
	csi <- par("csi")
	xbox <- pre$plotrange[1:2]
	ybox <- pre$plotrange[3:4]
	# Extend y range by 1% on top, and at least enough at bottom for labels and titles
	f <- fin[2L]/(fin[2L] - 4.2 * csi) - 0.99
	ybox <- c(extendrange(ybox, f=f)[1L], extendrange(ybox, f=0.01)[2L])
	# Extend x range to allocate left and right labels
	lmai <- pre$zax$margin * csi
	rmai <- select(pre$farcorn,
								 strwidth(pre$xax$labels[1L], units='inches', family='USGS'),
								 strwidth(pre$yax$labels[1L], units='inches', family='USGS'),
								 max(strwidth(pre$yax$labels, units='inches', family='USGS')),
								 max(strwidth(pre$xax$labels, units='inches', family='USGS')))
	rmai <- rmai + strwidth("  ", units='inches', family='USGS')
	ftot <- fin[1L]/(fin[1L] - lmai  - rmai) - 1.
	fl <- ftot * lmai/(lmai + rmai)
	fr <- ftot * rmai/(lmai + rmai)
	xbox <- c(extendrange(xbox, f=fl)[1L], extendrange(xbox, f=fr)[2L])
	xztit <- xbox[1L] # retain for z title
	# set par usr
	yrat <- fin[2L]/diff(ybox)
	xrat <- fin[1L]/diff(xbox)
	if(xrat/yrat > 1) {
		xbox <- extendrange(xbox, f=xrat/yrat - 1)
	} else
		ybox <- extendrange(ybox, f=yrat/xrat - 1)
	par(usr=c(xbox, ybox))
	# Get the transform matrix and the far corner
	trans <- pre$transform
	farcorn <- pre$farcorn
	# Prelims, draw back sides of box
	par(lwd=frameWt()) # lineweight for frame
	xlim=pre$xlim
	ylim=pre$ylim
	zlim=pre$zlim
	# the bottom (always the x-y plane) and orientation does not matter
	xgrid <- pre$xax$ticks
	for(xc in xgrid) {
		xyp <- cbind(c(xc,xc), ylim, zlim[c(1L, 1L)])
		topl <- xyp %*% trans
		lines(topl[,1L], topl[,2L])
	}
	# extract the angle of the y-axis to horizontal
	yang <- atan2(topl[2L, 2L] - topl[1L, 2L], topl[2L, 1L] - topl[1L, 1L]) * 180 / pi - 180
	ygrid <- pre$yax$ticks
	for(yc in ygrid) {
		xyp <- cbind(xlim, c(yc,yc), zlim[c(1L, 1L)])
		topl <- xyp %*% trans
		lines(topl[,1L], topl[,2L])
	}
	# extract the angle of the x-axis to horizontal
	xang <- atan2(topl[2L, 2L] - topl[1L, 2L], topl[2L, 1L] - topl[1L, 1L]) * 180 / pi
	# The x-z plane
	zgrid <- pre$zax$ticks
	if(farcorn %in% c(3L, 4L)) {
		ypck <- 2L
	} else
		ypck <- 1L
	for(xc in xgrid) {
		xzp <- cbind(c(xc,xc), ylim[c(ypck, ypck)], zlim)
		topl <- xzp %*% trans
		lines(topl[,1L], topl[,2L])
	}
	for(zc in zgrid) {
		xzp <- cbind(xlim, ylim[c(ypck, ypck)], c(zc,zc))
		topl <- xzp %*% trans
		lines(topl[,1L], topl[,2L])
	}
	# The y-z plane
	if(farcorn %in% c(2L, 4L)) {
		xpck <- 2L
	} else
		xpck <- 1L
	for(yc in ygrid) {
		yzp <- cbind(xlim[c(xpck,xpck)], c(yc,yc), zlim)
		topl <- yzp %*% trans
		lines(topl[,1L], topl[,2L])
	}
	for(zc in zgrid) {
		yzp <- cbind(xlim[c(xpck,xpck)], ylim, c(zc, zc))
		topl <- yzp %*% trans
		lines(topl[,1L], topl[,2L])
	}
	par(lwd=stdWt()) # set lineweight for remaining linework
	# OK, add labels, titles, and draw the surface
	# Sorry for spaghetti code, but once one worked, just copied and tweaked
	if(pre$farcorn == 4L) {
		# x on right
		xyz <- cbind(pre$xax$labelpos, ylim[1L], zlim[1L])
		topl <- xyz %*% trans
		if(pre$xax$style == "at") { # Draw at yangle
			text(topl[, 1L], topl[, 2L], labels=paste("  ", pre$xax$labels, sep="")
					 , srt=yang, adj=c(0,.5), family="USGS", cex=7/8)
			xoff <- max(strwidth(pre$xax$labels, family="USGS", units="user"))
			xoff <- xoff + strwidth("  ", family="USGS", units="user")
		} else { #Draw at xangle
			text(topl[, 1L], topl[, 2L], labels=pre$xax$labels, srt=xang, 
					 adj=c(.5,1.5), family="USGS", cex=7/8)
			xoff <- strheight(pre$xax$labels[1L], family="USGS", units="user")*2
		}
		# x title--center between orthogonal projection and in line with
		# the y grid lines, seems to work reasonably well.
		if(xtitle != "") {
			xsrt <- xang
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen + xoff*(cos(abs(yang)*pi/180) + sin(xang*pi/180))
			ypos <- ycen - xoff*(sin(abs(yang)*pi/180) + cos(xang*pi/180))
			text(xpos, ypos, 
					 labels=xtitle, srt=xsrt, family="USGS", adj=0.5, cex=9/8)
		}
		# y on left
		xyz <- cbind(xlim[1L], pre$yax$labelpos, zlim[1L])
		topl <- xyz %*% trans
		labels <- paste(pre$yax$labels, "  ", sep="")
		text(topl[, 1L], topl[, 2L], labels=labels, adj=c(1.0, 1.0),
				 srt=xang, family="USGS", cex=7/8)
		xoff <- max(strwidth(labels, family="USGS", units="user"))
		if(ytitle != "") {
			ysrt <- yang
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen - xoff*(cos(xang*pi/180) + sin(abs(yang)*pi/180))
			ypos <- ycen - xoff*(sin(xang*pi/180) + cos(abs(yang)*pi/180))
			text(xpos, ypos, 
					 labels=ytitle, srt=ysrt, family="USGS", adj=0.5, cex=9/8)
		}
		# z
		xyz <- cbind(xlim[1L], ylim[2L], pre$zax$labelpos)
		topl <- xyz %*% trans
		text(topl[, 1L], topl[, 2L], labels=paste(pre$zax$labels, " ", sep=""), adj=c(1,0),
				 family="USGS", cex=7/8)
		# z title
		if(ztitle != "") {
			ypos <- (topl[1L, 2L] + topl[nrow(topl), 2L])/2
			text(xztit, ypos, labels=ztitle, family="USGS", adj=c(0.5, 1.0), srt=90, cex=9/8)
		}
		Xseq <- seq(NXm1, 1L)
		Yseq <- seq(NYm1, 1L)
	} else if(pre$farcorn == 3L) {
		# x on left 
		xyz <- cbind(pre$xax$labelpos, ylim[1L], zlim[1L])
		topl <- xyz %*% trans
		if(pre$xax$style == "at") { # Draw at yangle
			text(topl[, 1L], topl[, 2L], labels=paste(pre$xax$labels, "  ", sep=""),
					 srt=yang-180, adj=c(1,1), family="USGS", cex=7/8)
			xoff <- max(strwidth(pre$xax$labels, family="USGS", units="user"))
			xoff <- xoff + strwidth("  ", family="USGS", units="user")
		} else { #Draw at xangle
			text(topl[, 1L], topl[, 2L], labels=pre$xax$labels, srt=xang, 
					 adj=c(.5,1.5), family="USGS", cex=7/8)
			xoff <- strheight(pre$xax$labels[1L], family="USGS", units="user")*2
		}
		# x title
		if(xtitle != "") {
			xsrt <- xang
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen + xoff*(cos(abs(yang)*pi/180) + sin(abs(xang)*pi/180))
			ypos <- ycen - xoff*(sin(abs(yang)*pi/180) + cos(abs(xang)*pi/180))
			text(xpos, ypos, 
					 labels=xtitle, srt=xsrt, family="USGS", adj=0.5, cex=9/8)
		}
		# y on right
		xyz <- cbind(xlim[2L], pre$yax$labelpos, zlim[1L])
		topl <- xyz %*% trans
		labels <- paste("  ", pre$yax$labels, sep="")
		text(topl[, 1L], topl[, 2L], labels=labels, adj=c(0.0, .5),
				 srt=xang, family="USGS", cex=7/8)
		xoff <- max(strwidth(labels, family="USGS", units="user"))
		if(ytitle != "") {
			ysrt <- yang - 180
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen + xoff*(cos(abs(xang)*pi/180) + sin(abs(yang)*pi/180))
			ypos <- ycen + xoff*(sin(abs(xang)*pi/180) + cos(abs(yang)*pi/180))
			text(xpos, ypos, 
					 labels=ytitle, srt=ysrt, family="USGS", adj=0.5, cex=9/8)
		}
		# z
		xyz <- cbind(xlim[1L], ylim[1L], pre$zax$labelpos)
		topl <- xyz %*% trans
		text(topl[, 1L], topl[, 2L], labels=paste(pre$zax$labels, " ", sep=""), adj=c(1,0),
				 family="USGS", cex=7/8)
		# z title
		if(ztitle != "") {
			ypos <- (topl[1L, 2L] + topl[nrow(topl), 2L])/2
			text(xztit, ypos, labels=ztitle, family="USGS", adj=c(0.5, 1.0), srt=90, cex=9/8)
		}
		Xseq <- seq(1L, NXm1)
		Yseq <- seq(NYm1, 1L)
	} else if(pre$farcorn == 2L) {
		# x on left 
		xyz <- cbind(pre$xax$labelpos, ylim[2L], zlim[1L])
		topl <- xyz %*% trans
		if(pre$xax$style == "at") { # Draw at yangle
			text(topl[, 1L], topl[, 2L], labels=paste(pre$xax$labels, "  ", sep=""),
					 srt=yang, adj=c(1,1), family="USGS", cex=7/8)
			xoff <- max(strwidth(pre$xax$labels, family="USGS", units="user"))
			xoff <- xoff + strwidth("  ", family="USGS", units="user")
		} else { #Draw at xangle
			text(topl[, 1L], topl[, 2L], labels=pre$xax$labels, srt=xang, 
					 adj=c(.5,1.5), family="USGS", cex=7/8)
			xoff <- strheight(pre$xax$labels[1L], family="USGS", units="user")*2
		}
		# x title
		if(xtitle != "") {
			xsrt <- xang - 180
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen - xoff*(cos(abs(yang)*pi/180) + sin(abs(xang)*pi/180))
			ypos <- ycen + xoff*(sin(abs(yang)*pi/180) + cos(abs(xang)*pi/180))
			text(xpos, ypos, 
					 labels=xtitle, srt=xsrt, family="USGS", adj=0.5, cex=9/8)
		}
		# y on right
		xyz <- cbind(xlim[1L], pre$yax$labelpos, zlim[1L])
		topl <- xyz %*% trans
		labels <- paste("  ", pre$yax$labels, sep="")
		text(topl[, 1L], topl[, 2L], labels=labels, adj=c(0.0, .5),
				 srt=xang - 180, family="USGS", cex=7/8)
		xoff <- max(strwidth(labels, family="USGS", units="user"))
		if(ytitle != "") {
			ysrt <- yang
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen - xoff*(cos(abs(xang)*pi/180) + sin(abs(yang)*pi/180))
			ypos <- ycen - xoff*(sin(abs(xang)*pi/180) + cos(abs(yang)*pi/180))
			text(xpos, ypos, 
					 labels=ytitle, srt=ysrt, family="USGS", adj=0.5, cex=9/8)
		}
		# z
		xyz <- cbind(xlim[2L], ylim[2L], pre$zax$labelpos)
		topl <- xyz %*% trans
		text(topl[, 1L], topl[, 2L], labels=paste(pre$zax$labels, " ", sep=""), adj=c(1,0),
				 family="USGS", cex=7/8)
		# z title
		if(ztitle != "") {
			ypos <- (topl[1L, 2L] + topl[nrow(topl), 2L])/2
			text(xztit, ypos, labels=ztitle, family="USGS", adj=c(0.5, 1.0), srt=90, cex=9/8)
		}
		Xseq <- seq(NXm1, 1L)
		Yseq <- seq(1L, NYm1)
	} else { # farcorn must be 1
		# x on right
		xyz <- cbind(pre$xax$labelpos, ylim[2L], zlim[1L])
		topl <- xyz %*% trans
		if(pre$xax$style == "at") { # Draw at yangle
			text(topl[, 1L], topl[, 2L], labels=paste("  ", pre$xax$labels, sep="")
					 , srt=yang-180, adj=c(0,.5), family="USGS", cex=7/8)
			xoff <- max(strwidth(pre$xax$labels, family="USGS", units="user"))
			xoff <- xoff + strwidth("  ", family="USGS", units="user")
		} else { #Draw at xangle
			text(topl[, 1L], topl[, 2L], labels=pre$xax$labels, srt=xang-180, 
					 adj=c(.5,1.5), family="USGS", cex=7/8)
			xoff <- strheight(pre$xax$labels[1L], family="USGS", units="user")*2
		}
		# x title
		if(xtitle != "") {
			xsrt <- xang - 180
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen - xoff*(cos(abs(yang)*pi/180) + sin(abs(xang)*pi/180))
			ypos <- ycen + xoff*(sin(abs(yang)*pi/180) + cos(abs(xang)*pi/180))
			text(xpos, ypos, 
					 labels=xtitle, srt=xsrt, family="USGS", adj=0.5, cex=9/8)
		}
		# y on left
		xyz <- cbind(xlim[2L], pre$yax$labelpos, zlim[1L])
		topl <- xyz %*% trans
		labels <- paste(pre$yax$labels, "  ", sep="")
		text(topl[, 1L], topl[, 2L], labels=labels, adj=c(1.0, 1.0),
				 srt=xang-180, family="USGS", cex=7/8)
		xoff <- max(strwidth(labels, family="USGS", units="user"))
		if(ytitle != "") {
			ysrt <- yang-180
			xcen <- (topl[nrow(topl), 1L] + topl[1L, 1L])/2
			ycen <- (topl[nrow(topl), 2L] + topl[1L, 2L])/2
			xpos <- xcen + xoff*(cos(abs(xang)*pi/180) + sin(abs(yang)*pi/180))
			ypos <- ycen + xoff*(sin(abs(xang)*pi/180) + cos(abs(yang)*pi/180))
			text(xpos, ypos, 
					 labels=ytitle, srt=ysrt, family="USGS", adj=0.5, cex=9/8)
		}
		# z
		xyz <- cbind(xlim[2L], ylim[1L], pre$zax$labelpos)
		topl <- xyz %*% trans
		text(topl[, 1L], topl[, 2L], labels=paste(pre$zax$labels, " ", sep=""), adj=c(1,0),
				 family="USGS", cex=7/8)
		# z title
		if(ztitle != "") {
			ypos <- (topl[1L, 2L] + topl[nrow(topl), 2L])/2
			text(xztit, ypos, labels=ztitle, family="USGS", adj=c(0.5, 1.0), srt=90, cex=9/8)
		}
		Xseq <- seq(1, NXm1)
		Yseq <- seq(1, NYm1)
	}
	border <- Surface$lineColor
	if(border == "none")
		border <- NA
	for(i in Xseq) {
		for(j in Yseq) {
			xyz <- cbind(c(x[i], x[i+1L], x[i+1L], x[i], x[i]),
									 c(y[j], y[j], y[j+1L], y[j+1L], y[j]),
									 c(z.surf[i,j], z.surf[i+1L,j], 
									 	z.surf[i+1L,j+1L], z.surf[i, j+1L], z.surf[i,j]))
			topl <- xyz %*% trans
			polygon(topl[,1L], topl[,2L], col=z.color[i,j],
							border=border, lwd=stdWt())
		}
	}
	pre$explanation <- list(contour=contour)
	invisible(pre)
}