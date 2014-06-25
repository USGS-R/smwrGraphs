# Set up for surface plot
#
# Coding history:
#    2014Jun09 DLLorenz Original Coding
#
preSurface <- function(x, y, z.surf, # The data
										zaxis.log = FALSE,  zaxis.range = c(NA, NA),
										yaxis.log = FALSE,  yaxis.range = c(NA, NA),
										xaxis.log = FALSE,  xaxis.range = c(NA, NA),
										xlabels="Auto", ylabels="Auto", zlabels="Auto",
										phi=NA, theta=NA, batch=FALSE) { # Axis set up
	# Check dimensions
	if(length(x) != nrow(z.surf))
		stop("the length of x must match the number of rows in z.surf")
	if(length(y) != ncol(z.surf))
		stop("the length of y must match the number of columns in z.surf")
	# Open a graphics window 
	setGD("preSurf")
	# Set up axis 
	zax <- setAxis(z.surf, zaxis.range, zaxis.log, FALSE, zlabels, 
								 extend.range=FALSE)
	zlim <- zax$dax$range
	z.surf <- zax$data
	yax <- setAxis(y, yaxis.range, yaxis.log, FALSE, ylabels, 
								 extend.range=FALSE)
	ylim <- yax$dax$range
	y <- yax$data
	if(isDateLike(x)) {
		xax <- list()
		xax$dax <- datePretty(x, major=xlabels)
		xax$data <- numericData(x)
	} else
		xax <- setAxis(x, xaxis.range, xaxis.log, FALSE, xlabels, 
									 extend.range=FALSE)
	xlim <- xax$dax$range
	x <- xax$data
	# Find a good angle for viewing if theta is NA
	if(is.na(theta)) {
		zmx <- whichRowCol(z.surf == max(z.surf))
		zmn <- whichRowCol(z.surf == min(z.surf))
		theta <- atan2(zmx[1,2] - zmn[1,2], zmx[1,1] - zmn[1,1])*180/pi
		theta <- theta-90
		theta <- round((theta - 15)/30)*30 + 15
		theta <- as.vector(theta) # Strip name
	}
	# Construct Theta vector
	th <- c(A=theta-15, B=theta, C=theta+15,
					D=theta-15, E=theta,"F"=theta+15,
					G=theta-15, H=theta, I=theta+15)
	# Construct the phi vector
	if(is.na(phi)) 
		phi=35
	ph <- c(A=phi+10, B=phi+10, C=phi+10,
					D=phi,    E=phi,    "F"=phi,
					G=phi-10, H=phi-10, I=phi-10)
	# Is the perspective good?
	par(mfrow=c(3,3))
	par(mar=c(.1, .1, .1, .1))
	retval <- list()
	for(i in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
		retval[[i]] <- persp(x, y, z.surf, theta=th[i], phi=ph[i], col="lightblue", 
												 xlim=xlim, ylim=ylim, zlim=zlim, d=1e6)
		pusr <- par("usr")
		text(pusr[1L] + (pusr[2L] - pusr[1L])/100,
				 pusr[4L] - (pusr[4L] - pusr[3L])/50,
				 paste(i, ": phi=", ph[i],  ", theta=", th[i], sep=""), adj=0)
	}
	if(is.character(batch)) {
		Resp <- batch
	} else if(batch) {
		Resp <- "E"
	} else {
		cat("Enter your choice A-I\n")
		Resp <- toupper(readline())
	}
	dev.off()
	if(!(Resp %in% LETTERS[1:9]))
		stop("Invalid selection")
	# Construct return values
	cube <- as.matrix(expand.grid(xlim,ylim,zlim))
	# The affine transformation matrix
	transform <- retval[[Resp]][1:3, 1:2]
	plotcube <- cube %*% transform
	# The far corner (the maximum y-value amongest the first 4 value)
	farcorn <- which.max(plotcube[1:4, 2L])
	# The range of the cube
	plotrange <- c(range(plotcube[, 1L]), range(plotcube[, 2L]))
	retval <- list(x=x, y=y, z=z.surf,
								 xax=xax$dax, yax=yax$dax, zax=zax$dax,
								 transform=transform, plotrange=plotrange,
								 xlim= xlim, ylim= ylim, zlim=zlim, farcorn=farcorn)
	invisible(retval)
}