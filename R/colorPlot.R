#' Plot Data
#' 
#' Produce a line/scatter plot where each point or group of related points has a
#'unique color or where sections along a line have different colors.
#' 
#' For linear axes, the range can be set to virtually any pair of values. For 
#'log axes, the choice of range is more resticted---for less than one log-cycle,
#'powers of whole numbers can be used; from 1 to about 3 log cycles, the choces 
#'should be powers of 3 or 10; and for more than 3 log cycles, the range sould be 
#'expressed only in powers of 10.
#'
#' @name colorPlot
#' @rdname colorPlot
#' @aliases colorPlot colorPlot,numeric,numeric-method
#'colorPlot,Date,numeric-method colorPlot,POSIXt,numeric-method
#' @param x the x-axis data
#' @param y the y-axis data
#' @param color the colors or a class to set colors, must match the length of
#' \code{x} and \code{y}.
#' @param Plot tagged list of control parameters of the plot: \code{name}="Auto"
#' means derive class names from the argument color, otherwise, must be a tagged 
#' list of color=name, ... (in which case the color tag is not used); \code{what}
#' can be only "points" or "lines" in the current version; \code{color}="Auto"
#' means if the argument \code{color} is double or dateLike create groups of classes,
#' otherwise create unique colors, alternate values are "Range" (treat like double),
#' tagged list of group_name=color, and so forth, "Discrete" valid only for numeric, or
#' "Index" valid only for integer or for specified colors.
#' @param yaxis.log logical, if \code{TRUE}, then log-transform the y axis
#' @param yaxis.rev logical, if \code{TRUE}, then reverse the y axis.
#' @param yaxis.range set the range of the y-axis. See \bold{Details}.
#' @param xaxis.log logical, if \code{TRUE}, then log-transform the x axis.
#' @param xaxis.range set the range of the x-axis. See \bold{Details}.
#' @param ylabels set up y-axis labels.
#' @param xlabels set up x-axis labels.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param ... arguments for specific methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{colorPlot}.
#' @docType methods
#' @section Methods: \describe{
#' \item{signature(x = "numeric", y = "numeric"))}{ Typically used to create a colored
#'scatter plot for numeric x and y data. }
#' \item{signature(x = "Date", y = "numeric")}{ Can be used to create a hydrograph
#'where the line is colored by a third variable, or a colored scatter plot over time. }
#' \item{signature(x = "POSIXt", y = "numeric")}{ Can be used to create a hydrograph
#'where the line is colored by a third variable, or a colored scatter plot over time. }
#'  }
#' @seealso \code{\link{setPage}}, \code{\link{xyPlot}}
#' @keywords methods hplot
#' @examples
#' \dontrun{
#' # See for examples of colorPlot:
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @exportMethod colorPlot
setGeneric("colorPlot", function(x, y, color, Plot=list(),
                              yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
                              xaxis.log=FALSE, xaxis.range=c(NA,NA),
                              ylabels=7, xlabels=7, xtitle="", ytitle="",
                              caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("colorPlot")
					 # Coding History:
					 #    2011Apr16 DLLorenz Original coding from xyPlot.
					 #    2011Aug03 DLLorenz Added axis labeling info to current
					 #    2011Oct24 DLLorenz Tweaks for package
					 #    2012Sep27 DLLorenz Made generic
					 #    2013Apr09 DLLorenz Added setGD
					 #    2014Jun25 DLLorenz Converted to roxygen
					 #
					 )

#' @rdname colorPlot
setMethod("colorPlot", signature("numeric", "numeric"), 
function(x, y, color, # data
         Plot=list(name="Auto", what="points", 
           symbol="circle", filled=TRUE,
           size=0.09, color="Auto", groups=4, ramp="greenRed"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA), # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels=7, xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), ...) { # margin controls
  ##
  ## set up the axes
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  if(dev.cur() == 1)
    setGD("ColorPlot")
  if(is.list(ylabels))
    yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  y <- yax$data
  yax <- yax$dax
  if(is.list(xlabels))
    xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=xlabels)
  
  xax <- do.call("setAxis", xax)
  x <- xax$data
  xax <- xax$dax
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ##
  plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  ## Set essential defaults to Plot
  if(is.null(Plot$name))
    Plot$name <- "Auto"
  if(is.null(Plot$color))
    Plot$color <- "Auto"
  if(is.null(Plot$ramp))
    Plot$ramp <- 'greenRed' # restore default
  if(is.null(Plot$groups))
  	Plot$groups <- 4
  ## Set up for multiplot
  ## Process color/Plot info
  ## Allow for simple index values for color numbers, no useable explanation
  SetRange <- FALSE
  if(is.character(Plot$color) && Plot$color=="Index") {
    Plot <- setPlot(list(name=Plot$name, what=Plot$what, symbol=Plot$symbol,
                         size=Plot$size), name="", what='points', type='solid',
                    width='standard', symbol='circle', filled=TRUE,
                    size=0.09, color='black') # Force defaults if not set
    parms <- list(Explan=setExplan(Plot)) # Add info to set up explanation
    plotPars <- parms$Explan$current
    color <- setColor(color) # Force valid colors
    points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
           pch=plotPars$pch, cex=plotPars$cex, col=color, bg=color)
  }
  else {
    ## How to process color--populate name and color tags in Plot
    ColorClass <- class(color)
    ColorClass <- ColorClass[length(ColorClass)] # use the last one (most generic)
    ColorNumeric <- ColorClass %in% c("numeric", "integer", "POSIXt", "Date")
    if(ColorNumeric && Plot$color == "Discrete") {
      ## Force to character and use discrete values as the color codes
      color <- as.character(color)
      Plot$name <- color
      ColorNdx <- sort(unique(color))
      ColorVals <- rainbow(length(ColorNdx), end=2/3)
      names(ColorVals) <- ColorNdx
      Plot$color <- ColorVals[color]
    }
    else if(ColorNumeric && Plot$color == "Range") {
      SetRange <- TRUE
      ## Create groups
      ColorRange <- range(color, na.rm=TRUE) # Record range for explanation
      if(!(Plot$ramp %in% c("gray", "grey"))) # the gray scale has a different name
        ColorRamp <- get(paste(Plot$ramp, 'colors', sep='.')) # The function
      else
        ColorRamp <- gray
      ColorN <- min(Plot$groups, length(unique(color)))
      # Equal ranges
      ColorBrk <- quantile(ColorRange, probs=seq(0, ColorN) / ColorN, type=7)
      color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
      ColorNdx <- rev(levels(color))
      ## Assign levels to name and colors to color
      Plot$name <- as.character(color)
      ColorVals <- ColorRamp(length(levels(color)))
      names(ColorVals) <- levels(color)
      Plot$color <- ColorVals[color]
    }
    else if(ColorNumeric && Plot$color == "Auto") {
    	if(!(Plot$ramp %in% c("gray", "grey"))) # the gray scale has a different name
    		ColorRamp <- get(paste(Plot$ramp, 'colors', sep='.')) # The function
    	else
    		ColorRamp <- gray
    	ColorBrk <- pretty(range(color, na.rm=TRUE), Plot$groups)
    	Plot$groups <- length(ColorBrk) - 1L
      color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
      ColorNdx <- rev(levels(color))
      ## Assign colors to name
      Plot$name <- as.character(color)
      ColorVals <- ColorRamp(Plot$groups)
      names(ColorVals) <- levels(color)
      Plot$color <- ColorVals[color]
    } # Done with numeric ColorClass
    else if(is.character(Plot$name)) { # Only valid character option is Auto, assumed
      ## Check color spcification
      if(is.character(Plot$color)) { # Only valid character option is Auto, assumed
        ## Create colors
        color <- as.factor(color)
        ColorNdx <- levels(color)
        ## Assign colors to name
        Plot$name <- as.character(color)
        ColorVals <- rainbow(length(levels(color)), end=2/3)
        names(ColorVals) <- levels(color)
        Plot$color <- ColorVals[color]
      }
      else { # Plot$color must be a list (or named vector)
        color <- as.character(color)
        ColorNdx <- names(Plot$color)
        Plot$name <- color
        Plot$color <- unlist(Plot$color[color])
      }
    }
    else { # Plot$name must be a list (or named vector)
      color <- as.character(color)
      ColorNdx <- as.vector(unlist(Plot$name))
      Plot$color <- unlist(Plot$name[color])
      Plot$name <- color
    }
    ## Done with all of the color processing
    ## Create the plot
    if(Plot$what == "points") {
    	parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
    												width="standard", symbol="circle", filled=TRUE,
    												size=0.09, color="black", order=ColorNdx)
    	points(x, y, type="p", pch=parms$current$pch, cex=parms$current$cex,
    				 col=parms$current$col, bg=parms$current$col)
    	## Fix color range to show only the min and max for points
    	if(SetRange) {
    		Plot$name <- rev(paste(signif(ColorRange, 3), c("(min)", "(max)"), sep=" "))
    		Plot$color <- ColorVals[c(length(ColorVals), 1)]
    		parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
    													width="standard", symbol="circle", filled=TRUE,
    													size=0.09, color="black")
    	}
    } else if(Plot$what == "lines") {
    	parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
    												width="color", symbol="circle", filled=TRUE,
    												size=0.09, color="black", order=ColorNdx)
    	Nxy <- length(x)
    	segments(x[-Nxy], y[-Nxy], x[-1L], y[-1L], col=parms$current$col, lwd=parms$current$lwd)
    	# Set Explanation as with contour plot if ramp
    	if(ColorNumeric) {
    		parms$Explan <- NULL
    		name <- Plot$name
    		if(name == "Auto")
    			name <- ""
    		parms$Explan$contour <- list(zvalues=matrix(ColorBrk[-1L] - diff(ColorBrk)/2, nrow=1),
    																 fillcol=ColorVals,
    																 breaks=ColorBrk,
    																 xvals=c(0.10, 0.35),
    																 yvals=seq(to=.5, by=-.25, length.out=length(ColorBrk)),
    																 linecol="none",
    																 name=name)
    	}
    } else
    	stop("Only points or lines can be drawn in the current version")
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, color=color, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                  xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
                  yax=yax, xax=xax)))
}
)

#' @rdname colorPlot
setMethod("colorPlot", signature("Date", "numeric"), 
function(x, y, color, # data
				 Plot=list(name="Auto", what="lines", 
				 					symbol="circle", filled=TRUE,
				 					size=0.09, color="Auto", groups=10, ramp="greenRed"), # plot controls
				 yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA), # y-axis controls
				 xaxis.log=FALSE, xaxis.range=range(x, na.rm=TRUE), # x-axis controls
				 ylabels=7, xlabels="Auto", # labels
				 xtitle="",
				 ytitle=deparse(substitute(y)), # axis titles
				 caption="", # caption 
				 margin=c(NA, NA, NA, NA), ...) { # margin controls
	##
	## set up the axes
	xtitle=xtitle # needed to 'set' names
	ytitle=ytitle
	if(dev.cur() == 1)
		setGD("ColorPlot")
	if(is.list(ylabels))
		yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
									axis.rev=yaxis.rev), ylabels)
	else
		yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
								axis.rev=yaxis.rev, axis.labels=ylabels)
	yax <- do.call("setAxis", yax)
	y <- yax$data
	yax <- yax$dax
	dax <- datePretty(xaxis.range, major=xlabels)
	x <- numericData(x)
	## set margins and controls
	margin.control <- setMargin(margin, yax)
	margin <- margin.control$margin
	right <- margin.control$right
	top <- margin.control$top
	left <- margin.control$left
	bot <- margin.control$bot
	par(mar=margin)
	##
	plot(x, y, type='n', xlim=dax$range, xaxs='i', axes=FALSE,
			 ylim=yax$range, yaxs='i', ylab="", xlab="")
	## Set essential defaults to Plot
	if(is.null(Plot$name))
		Plot$name <- "Auto"
	if(is.null(Plot$color))
		Plot$color <- "Auto"
	if(is.null(Plot$ramp))
		Plot$ramp <- 'greenRed' # restore default
	if(is.null(Plot$groups))
		Plot$groups <- 4
	## Set up for multiplot
	## Process color/Plot info
	## Allow for simple index values for color numbers, no useable explanation
	SetRange <- FALSE
	if(is.character(Plot$color) && Plot$color=="Index") {
		Plot <- setPlot(list(name=Plot$name, what=Plot$what, symbol=Plot$symbol,
												 size=Plot$size), name="", what='points', type='solid',
										width='standard', symbol='circle', filled=TRUE,
										size=0.09, color='black') # Force defaults if not set
		parms <- list(Explan=setExplan(Plot)) # Add info to set up explanation
		plotPars <- parms$Explan$current
		color <- setColor(color) # Force valid colors
		points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
					 pch=plotPars$pch, cex=plotPars$cex, col=color, bg=color)
	}
	else {
		## How to process color--populate name and color tags in Plot
		ColorClass <- class(color)
		ColorClass <- ColorClass[length(ColorClass)] # use the last one (most generic)
		ColorNumeric <- ColorClass %in% c("numeric", "integer", "POSIXt", "Date")
		if(ColorNumeric && Plot$color == "Discrete") {
			## Force to character and use discrete values as the color codes
			color <- as.character(color)
			Plot$name <- color
			ColorNdx <- sort(unique(color))
			ColorVals <- rainbow(length(ColorNdx), end=2/3)
			names(ColorVals) <- ColorNdx
			Plot$color <- ColorVals[color]
		}
		else if(ColorNumeric && Plot$color == "Range") {
			SetRange <- TRUE
			## Create groups
			ColorRange <- range(color, na.rm=TRUE) # Record range for explanation
			if(!(Plot$ramp %in% c("gray", "grey"))) # the gray scale has a different name
				ColorRamp <- get(paste(Plot$ramp, 'colors', sep='.')) # The function
			else
				ColorRamp <- gray
			ColorN <- min(Plot$groups, length(unique(color)))
			# Equal ranges
			ColorBrk <- quantile(ColorRange, probs=seq(0, ColorN) / ColorN, type=7)
			color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
			ColorNdx <- rev(levels(color))
			## Assign levels to name and colors to color
			Plot$name <- as.character(color)
			ColorVals <- ColorRamp(length(levels(color)))
			names(ColorVals) <- levels(color)
			Plot$color <- ColorVals[color]
		}
		else if(ColorNumeric && Plot$color == "Auto") {
			if(!(Plot$ramp %in% c("gray", "grey"))) # the gray scale has a different name
				ColorRamp <- get(paste(Plot$ramp, 'colors', sep='.')) # The function
			else
				ColorRamp <- gray
			ColorBrk <- pretty(range(color, na.rm=TRUE), Plot$groups)
			Plot$groups <- length(ColorBrk) - 1L
			color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
			ColorNdx <- rev(levels(color))
			## Assign colors to name
			Plot$name <- as.character(color)
			ColorVals <- ColorRamp(Plot$groups)
			names(ColorVals) <- levels(color)
			Plot$color <- ColorVals[color]
		} # Done with numeric ColorClass
		else if(is.character(Plot$name)) { # Only valid character option is Auto, assumed
			## Check color spcification
			if(is.character(Plot$color)) { # Only valid character option is Auto, assumed
				## Create colors
				color <- as.factor(color)
				ColorNdx <- levels(color)
				## Assign colors to name
				Plot$name <- as.character(color)
				ColorVals <- rainbow(length(levels(color)), end=2/3)
				names(ColorVals) <- levels(color)
				Plot$color <- ColorVals[color]
			}
			else { # Plot$color must be a list (or named vector)
				color <- as.character(color)
				ColorNdx <- names(Plot$color)
				Plot$name <- color
				Plot$color <- unlist(Plot$color[color])
			}
		}
		else { # Plot$name must be a list (or named vector)
			color <- as.character(color)
			ColorNdx <- as.vector(unlist(Plot$name))
			Plot$color <- unlist(Plot$name[color])
			Plot$name <- color
		}
		## Done with all of the color processing
		## Create the plot
		if(Plot$what == "points") {
			parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
														width="standard", symbol="circle", filled=TRUE,
														size=0.09, color="black", order=ColorNdx)
			points(x, y, type="p", pch=parms$current$pch, cex=parms$current$cex,
						 col=parms$current$col, bg=parms$current$col)
			## Fix color range to show only the min and max for points
			if(SetRange) {
				Plot$name <- rev(paste(signif(ColorRange, 3), c("(min)", "(max)"), sep=" "))
				Plot$color <- ColorVals[c(length(ColorVals), 1)]
				parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
															width="standard", symbol="circle", filled=TRUE,
															size=0.09, color="black")
			}
		} else if(Plot$what == "lines") {
			parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
														width="color", symbol="circle", filled=TRUE,
														size=0.09, color="black", order=ColorNdx)
			Nxy <- length(x)
			segments(x[-Nxy], y[-Nxy], x[-1L], y[-1L], col=parms$current$col, lwd=parms$current$lwd)
			# Set Explanation as with contour plot if ramp
			if(ColorNumeric) {
				parms$Explan <- NULL
				name <- Plot$name
				if(name == "Auto")
					name <- ""
				parms$Explan$contour <- list(zvalues=matrix(ColorBrk[-1L] - diff(ColorBrk)/2, nrow=1),
																		 fillcol=ColorVals,
																		 breaks=ColorBrk,
																		 xvals=c(0.10, 0.35),
																		 yvals=seq(to=.5, by=-.25, length.out=length(ColorBrk)),
																		 linecol="none",
																		 name=name)
			}
		} else
			stop("Only points or lines can be drawn in the current version")
	}
	box(lwd=frameWt())
	## label the axes
	renderY(yax, lefttitle=ytitle, left=left, right=right)
	renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
	invisible((list(x=x, y=y, color=color, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
									xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
									yax=yax, xax=dax)))
}
)

#' @rdname colorPlot
setMethod("colorPlot", signature("POSIXt", "numeric"), 
function(x, y, color, # data
				 Plot=list(name="Auto", what="lines", 
				 					symbol="circle", filled=TRUE,
				 					size=0.09, color="Auto", groups=10, ramp="greenRed"), # plot controls
				 yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA), # y-axis controls
				 xaxis.log=FALSE, xaxis.range=range(x, na.rm=TRUE), # x-axis controls
				 ylabels=7, xlabels="Auto", # labels
				 xtitle="",
				 ytitle=deparse(substitute(y)), # axis titles
				 caption="", # caption 
				 margin=c(NA, NA, NA, NA), ...) { # margin controls
	## 
	## set up the axes
	xtitle=xtitle # needed to 'set' names
	ytitle=ytitle
	if(dev.cur() == 1)
		setGD("ColorPlot")
	if(is.list(ylabels))
		yax <- c(list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
									axis.rev=yaxis.rev), ylabels)
	else
		yax <- list(data=y, axis.range=yaxis.range, axis.log=yaxis.log,
								axis.rev=yaxis.rev, axis.labels=ylabels)
	yax <- do.call("setAxis", yax)
	y <- yax$data
	yax <- yax$dax
	dax <- datePretty(xaxis.range, major=xlabels)
	x <- numericData(x)
	## set margins and controls
	margin.control <- setMargin(margin, yax)
	margin <- margin.control$margin
	right <- margin.control$right
	top <- margin.control$top
	left <- margin.control$left
	bot <- margin.control$bot
	par(mar=margin)
	##
	plot(x, y, type='n', xlim=dax$range, xaxs='i', axes=FALSE,
			 ylim=yax$range, yaxs='i', ylab="", xlab="")
	## Set essential defaults to Plot
	if(is.null(Plot$name))
		Plot$name <- "Auto"
	if(is.null(Plot$color))
		Plot$color <- "Auto"
	if(is.null(Plot$ramp))
		Plot$ramp <- 'greenRed' # restore default
	if(is.null(Plot$groups))
		Plot$groups <- 4
	## Set up for multiplot
	## Process color/Plot info
	## Allow for simple index values for color numbers, no useable explanation
	SetRange <- FALSE
	if(is.character(Plot$color) && Plot$color=="Index") {
		Plot <- setPlot(list(name=Plot$name, what=Plot$what, symbol=Plot$symbol,
												 size=Plot$size), name="", what='points', type='solid',
										width='standard', symbol='circle', filled=TRUE,
										size=0.09, color='black') # Force defaults if not set
		parms <- list(Explan=setExplan(Plot)) # Add info to set up explanation
		plotPars <- parms$Explan$current
		color <- setColor(color) # Force valid colors
		points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
					 pch=plotPars$pch, cex=plotPars$cex, col=color, bg=color)
	}
	else {
		## How to process color--populate name and color tags in Plot
		ColorClass <- class(color)
		ColorClass <- ColorClass[length(ColorClass)] # use the last one (most generic)
		ColorNumeric <- ColorClass %in% c("numeric", "integer", "POSIXt", "Date")
		if(ColorNumeric && Plot$color == "Discrete") {
			## Force to character and use discrete values as the color codes
			color <- as.character(color)
			Plot$name <- color
			ColorNdx <- sort(unique(color))
			ColorVals <- rainbow(length(ColorNdx), end=2/3)
			names(ColorVals) <- ColorNdx
			Plot$color <- ColorVals[color]
		}
		else if(ColorNumeric && Plot$color == "Range") {
			SetRange <- TRUE
			## Create groups
			ColorRange <- range(color, na.rm=TRUE) # Record range for explanation
			if(!(Plot$ramp %in% c("gray", "grey"))) # the gray scale has a different name
				ColorRamp <- get(paste(Plot$ramp, 'colors', sep='.')) # The function
			else
				ColorRamp <- gray
			ColorN <- min(Plot$groups, length(unique(color)))
			# Equal ranges
			ColorBrk <- quantile(ColorRange, probs=seq(0, ColorN) / ColorN, type=7)
			color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
			ColorNdx <- rev(levels(color))
			## Assign levels to name and colors to color
			Plot$name <- as.character(color)
			ColorVals <- ColorRamp(length(levels(color)))
			names(ColorVals) <- levels(color)
			Plot$color <- ColorVals[color]
		}
		else if(ColorNumeric && Plot$color == "Auto") {
			if(!(Plot$ramp %in% c("gray", "grey"))) # the gray scale has a different name
				ColorRamp <- get(paste(Plot$ramp, 'colors', sep='.')) # The function
			else
				ColorRamp <- gray
			ColorBrk <- pretty(range(color, na.rm=TRUE), Plot$groups)
			Plot$groups <- length(ColorBrk) - 1L
			color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
			ColorNdx <- rev(levels(color))
			## Assign colors to name
			Plot$name <- as.character(color)
			ColorVals <- ColorRamp(Plot$groups)
			names(ColorVals) <- levels(color)
			Plot$color <- ColorVals[color]
		} # Done with numeric ColorClass
		else if(is.character(Plot$name)) { # Only valid character option is Auto, assumed
			## Check color spcification
			if(is.character(Plot$color)) { # Only valid character option is Auto, assumed
				## Create colors
				color <- as.factor(color)
				ColorNdx <- levels(color)
				## Assign colors to name
				Plot$name <- as.character(color)
				ColorVals <- rainbow(length(levels(color)), end=2/3)
				names(ColorVals) <- levels(color)
				Plot$color <- ColorVals[color]
			}
			else { # Plot$color must be a list (or named vector)
				color <- as.character(color)
				ColorNdx <- names(Plot$color)
				Plot$name <- color
				Plot$color <- unlist(Plot$color[color])
			}
		}
		else { # Plot$name must be a list (or named vector)
			color <- as.character(color)
			ColorNdx <- as.vector(unlist(Plot$name))
			Plot$color <- unlist(Plot$name[color])
			Plot$name <- color
		}
		## Done with all of the color processing
		## Create the plot
		if(Plot$what == "points") {
			parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
														width="standard", symbol="circle", filled=TRUE,
														size=0.09, color="black", order=ColorNdx)
			points(x, y, type="p", pch=parms$current$pch, cex=parms$current$cex,
						 col=parms$current$col, bg=parms$current$col)
			## Fix color range to show only the min and max for points
			if(SetRange) {
				Plot$name <- rev(paste(signif(ColorRange, 3), c("(min)", "(max)"), sep=" "))
				Plot$color <- ColorVals[c(length(ColorVals), 1)]
				parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
															width="standard", symbol="circle", filled=TRUE,
															size=0.09, color="black")
			}
		} else if(Plot$what == "lines") {
			parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
														width="color", symbol="circle", filled=TRUE,
														size=0.09, color="black", order=ColorNdx)
			Nxy <- length(x)
			segments(x[-Nxy], y[-Nxy], x[-1L], y[-1L], col=parms$current$col, lwd=parms$current$lwd)
			# Set Explanation as with contour plot if ramp
			if(ColorNumeric) {
				parms$Explan <- NULL
				name <- Plot$name
				if(name == "Auto")
					name <- ""
				parms$Explan$contour <- list(zvalues=matrix(ColorBrk[-1L] - diff(ColorBrk)/2, nrow=1),
																		 fillcol=ColorVals,
																		 breaks=ColorBrk,
																		 xvals=c(0.10, 0.35),
																		 yvals=seq(to=.5, by=-.25, length.out=length(ColorBrk)),
																		 linecol="none",
																		 name=name)
			}
		} else
			stop("Only points or lines can be drawn in the current version")
	}
	box(lwd=frameWt())
	## label the axes
	renderY(yax, lefttitle=ytitle, left=left, right=right)
	renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
	invisible((list(x=x, y=y, color=color, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
									xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
									yax=yax, xax=dax)))
}
)
