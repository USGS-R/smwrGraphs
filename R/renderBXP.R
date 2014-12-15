#' Box Plot
#' 
#' Draw a box plot (support functions).
#' 
#' 
#' @aliases renderBXP renderBoxPlot
#' @param x the x-coordinate of the box.
#' @param width the width of the box in x-axis units.
#' @param z a list containing the statistics for the individual boxplot.
#' @param fill a character string describing the fill color for the box or
#' "none" for no fill.
#' @param draw.RL draw the reporting level?
#' @param xtoplot the x-axis locations for each boxplot.
#' @param stats a list containing the statistics for the boxplots.
#' @param Box a list containing the control info for the boxplots.
#' @param explan a list containing the information for an explanation.
#' @param expz a list containing the information for an explanation of the
#' boxplot.
#' @param yaxis.log logical: use a log transfor for the data nad the y-axis?
#' @param yrange set the y-axis range.
#' @param xrange set the x-axis range.
#' @param ylabels either "Auto," the approximate number of labels, or the
#' actual labels to use for the y-axis.
#' @param xlabels either "Auto" or the x-axis labels for each boxplot.
#' @param xlabels.rotate logical: rotate the x-axis lables by 90 degrees?
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @return Information about the graph.
#' @note The function \code{renderBXP} draws a single boxplot in a graph. The
#' function \code{renderBoxPlot} is called from each method function for
#' \code{boxPlot} to produce the boxplot.
#' @seealso \code{\link{boxPlot}}
#' @keywords aplot
#' @rdname renderBXP
#' @export
renderBoxPlot <- function(xtoplot, stats, Box, explan, expz, # from compute stats + Box
													yaxis.log=FALSE, yrange=c(NA,NA), # y-axis controls
													xrange=range(xtoplot) + c(-1, 1),
													ylabels='Auto', xlabels='Auto', xlabels.rotate=FALSE, # labels
													xtitle='', ytitle='',  caption='', # axis titles and caption
													margin=c(NA,NA,NA,NA)) { # margin control
	# Coding history:
	#    2008Jun17 DLLorenz Original version based on S function boxes
	#    2011Dec22 DLLorenz Modified for R and new structure of data--extracted
	#                        the renderBoxPlot function from the old boxPlot fcn.
	#    2012Jan18 DLLorenz Bug fix to suppress plotting boxes if n = 0
	#    2012Feb15 DLLorenz Begin mods for new pub standard
	#    2012Sep18 DLLorenz Added fill option to box.
	#    2012Oct31 DLLorenz Changed outside value to symbol 21
	#    2012Nov01 DLLorenz Add options to produce circles in PDF output
	#    2013Apr09 DLLorenz Added setGD 
	#    2013Aug15 DLLorenz Bug fix in outside value logic
	#    2014Jun26 DLLorenz Converted to roxygen
	##
	## Preliminaries
	xrange <- xrange
	if(dev.cur() == 1)
		setGD("BoxPlot")
	size.lab <- 0.7 # size of outlier symbols
	mindat <- stats$data.range[1L]
	maxdat <- stats$data.range[2L]
	stats$data.range <- NULL # Not needed, must discard for processing boxes
	if(any(is.na(yrange))) {
		yrange <- c(mindat, maxdat)
		hard <- FALSE
		if(Box$show.counts)
			if(yaxis.log)
				yrange <- yrange + c(0, 0.0414) * diff(yrange)
		else
			yrange <- yrange * c(1, 1.10)
	}
	else
		hard <- TRUE # setting yrange forces hard limits
	if(yaxis.log)
		yax <- logPretty(yrange, labels=ylabels, hard=hard)
	else
		yax <- linearPretty(yrange, labels=ylabels, hard=hard)
	yrange <- yax$range # get range from the set up routine
	## Set up for rotated axis labels
	if(xlabels[1L] != "Auto") 
		parnames <- xlabels
	else
		parnames <- names(stats)
	nbox <- length(xtoplot)
	## Set margins and controls
	if(xlabels.rotate) {
		botmar <- max(strwidth(parnames, units='inches', family='USGS'))/par('cin')[2L] + 2.2
		if(is.na(margin[1L]))
			margin[1L] <- pmax(3.2, botmar)
	}
	margin.control <- setMargin(margin, yax)
	margin <- margin.control$margin
	right <- margin.control$right
	top <- margin.control$top
	left <- margin.control$left
	bot <- margin.control$bot
	if(xlabels.rotate)
		bot$angle <- 90 # Use this logic
	if(is.numeric(xlabels)) { # build the x-axis labels
		xax <- linearPretty(xrange, labels=xlabels)
		xaxis.lev <- NULL
	} else {
		xax <- namePretty(parnames, orientation='grid', offset=1)
		bot$ticks <- top$ticks <- FALSE
		## get the levels
		xaxis.lev <- xax$labels
	}
	xrange <- xax$range
	meanspacing <- diff(xrange)/(nbox + 1)
	par(mar=margin)
	## set up the boxplot, use renderBXP
	par(lwd=stdWt()) # standard line weight
	plot(mean(xrange), mean(yrange), type='n', xlim=xrange, xaxs='i', axes=FALSE,
			 ylim=yrange, yaxs='i', ylab="", xlab="")
	ux <- par('pin')[1L]/diff(par('usr')[1:2]) # the inches per x-usr unit
	if(Box$width == 'Auto') # set width to a maximum of .5 inch or based on spacing
		width <- min(.5/ux,  meanspacing/1.5)
	else
		width=Box$width/ux
	nobs <- integer(nbox)
	for(i in seq(nbox)) {
		if(stats[[i]]$n > Box$nobox)
			stats[[i]] <- renderBXP(xtoplot[i], width, stats[[i]],
															is.na(Box$censorbox), Box$fill)
		else if(stats[[i]]$n > 0) { # circles if data, nothing if no data
			## Set color tweak for PDF output for points
			## See https://stat.ethz.ch/pipermail/r-help/2007-October/144598.html
			if(!is.null(options(".pdf_graph")$.pdf_graph) && options(".pdf_graph")$.pdf_graph)
				symColor <- "#000000FE"
			else
				symColor <- "#000000"
			pts <- stats[[i]]$out
			points(rep(xtoplot[i], length(pts)), pts, pch=21, cex=par("cex")*1.2,
						 col=symColor)
		}
		nobs[i] <- stats[[i]]$n
	}
	## make labels if necessary
	if(xlabels[1L] != "Auto") 
		parnames <- xlabels
	renderX(xax, bottitle=xtitle, caption=caption,
					bottom=bot, top=top)
	renderY(yax, lefttitle=ytitle, left=left, right=right)
	if(Box$show.counts)
		mtext(3, line=-1.4, at=xtoplot, text=as.character(nobs), family='USGS')
	box(lwd=frameWt())
	resbox <- list(stats=stats)
	resbox$positions <- xtoplot
	resbox$width <- width*ux # back to inches
	resbox$margin <- margin
	resbox$type <- Box$type
	## insert axis controls
	resbox$yaxis.log <- yaxis.log
	resbox$yaxis.rev <- FALSE
	resbox$xaxis.log <- FALSE
	resbox$xaxis.lev <- xaxis.lev
	explanation <- c(explan, expz, n='32')
	if(!Box$show.counts) { # remove that from the stuff to draw
		explanation$n <- NULL
		explanation$labels <- explanation$labels[-1L]
		explanation$values <- explanation$values[-1L]
	}
	explanation$width <- width * ux
	if(Box$nobox > min(nobs))
		explanation$comment <- paste("Boxes only drawn for more than", Box$nobox,
																 "data values", sep=' ')
	resbox$explanation <- explanation
	resbox$yax <- yax
	resbox$xax <- xax
	invisible(resbox)
}

#' @rdname renderBXP
#' @export renderBXP
renderBXP <- function(x, width, z, draw.RL=TRUE, fill="none") {
  ## The workhorse function taht actuallu draws the boxes
  ##
  ## Fix farout, estimated and censored if missing--allows ouput from regular
  ## boxplot for a single variable to work.
  if(is.null(z$censored))
    z$censored <- -Inf
  if(is.null(z$estimated))
    z$estimated <- -Inf
  if(is.null(z$farout))
    z$farout <- numeric(0)
  ## draw box and whiskers
  xmd <- x - 0.5 * width # the horizontal offest for the box
  xpd <- x + 0.5 * width
  y <- as.vector(z$stats)
  ck <- y < z$censored
  if(any(ck))
    y[ck] <- z$censored # truncate at censored level
  ## box
  if(fill == "none")
    den <- 0
  else
    den <- NA
  if(y[4L] < z$estimated) # draw the whole in gray
    polygon(c(xmd, xmd, xpd, xpd), c(y[2L], y[4L], y[4L], y[2L]),
            density=den, col=fill, border="gray50", lwd=lineWt('standard'))
  else # draw in black and over print if necessary
    polygon(c(xmd, xmd, xpd, xpd), c(y[2L], y[4L], y[4L], y[2L]),
            density=den, col=fill, border="black", lwd=lineWt('standard'))
  ## Overplot with lines if estimated
  if(y[2L] < z$estimated) {
    yMaxGray <- min(z$estimated, y[4L]) # Do not draw above the box
    segments(xmd, yMaxGray, xmd, y[2L], lwd=lineWt('standard'), col="gray50")
    segments(xmd, y[2L], xpd, y[2L], lwd=lineWt('standard'), col="gray50")
    segments(xpd, yMaxGray, xpd, y[2L], lwd=lineWt('standard'), col="gray50")
  }
  ## median
  if(y[3L] < z$estimated) # gray for estimated
    segments(xmd, y[3L], xpd, y[3L], lwd=lineWt('standard'), col="gray50")
  else
    segments(xmd, y[3L], xpd, y[3L], lwd=lineWt('standard'), col="black")
  ## whiskers with end caps
  if(y[2L] < z$estimated) {
    segments(x, y[1L], x, y[2L], lwd=lineWt('standard'), col="gray50")
    segments(x - 0.5 * width, y[1L], x + 0.5 * width, y[1L],
             lwd=lineWt('standard'), col="gray50")
  }
  else if(y[1L] < z$estimated) {
    segments(x, y[1L], x, z$estimated, lwd=lineWt('standard'), col="gray50")
    segments(x, z$estimated, x, y[2L], lwd=lineWt('standard'), col="black")
    segments(x - 0.5 * width, y[1L], x + 0.5 * width, y[1L],
             lwd=lineWt('standard'), col="gray50")
  }
  else {
    segments(x, y[1L], x, y[2L], lwd=lineWt('standard'), col="black")
    if(!ck[1L]) # do not end cap draw if censored 
      segments(x - 0.5 * width, y[1L], x + 0.5 * width, y[1L],
               lwd=lineWt('standard'), col="black")
  }
  if(y[5L] < z$estimated) {
    segments(x, y[4L], x, y[5L], lwd=lineWt('standard'), col="gray50")
    segments(x - 0.5 * width, y[5L], x + 0.5 * width, y[5L],
             lwd=lineWt('standard'), col="gray50")
  }
  else if(y[4L] < z$estimated) {
    segments(x, y[4L], x, z$estimated, lwd=lineWt('standard'), col="gray50")
    segments(x, z$estimated, x, y[5L], lwd=lineWt('standard'), col="black")
    segments(x - 0.5 * width, y[5L], x + 0.5 * width, y[5L],
             lwd=lineWt('standard'), col="black")
  }
  else {
    segments(x, y[4L], x, y[5L], lwd=lineWt('standard'), col="black")
    segments(x - 0.5 * width, y[5L], x + 0.5 * width, y[5L],
             lwd=lineWt('standard'), col="black")
  }
  ## Outside values--logic needed for either out & farout not present or
  ##  numeric(0)
  if(is.null(z$out) || length(z$out) == 0)
    pick <- FALSE
  else
    pick <- (z$out > z$censored & z$out > z$estimated)
  if(any(pick)) {
    ## Set color tweak for PDF output for points
    ## See https://stat.ethz.ch/pipermail/r-help/2007-October/144598.html
  	if(!is.null(options(".pdf_graph")$.pdf_graph) && options(".pdf_graph")$.pdf_graph)
  		symColor <- "#000000FE"
  	else
  		symColor <- "#000000"
  	out <- z$out[pick]
  	points(rep(x, length(out)), out, pch=21, cex=par("cex")*1.2, col=symColor)
  }
  ## Estimated outside values
  if(is.null(z$out) || length(z$out) == 0)
    pick <- FALSE
  else
    pick <- (z$out > z$censored & z$out < z$estimated)
  if(any(pick)) {
    ## Set color tweak for PDF output for points
    ## See https://stat.ethz.ch/pipermail/r-help/2007-October/144598.html
  	if(!is.null(options(".pdf_graph")$.pdf_graph) && options(".pdf_graph")$.pdf_graph)
  		symColor <- "#808080FE"
  	else
  		symColor <- "#808080"
  	out <- z$out[pick]
  	points(rep(x, length(out)), out, pch=21, cex=par("cex")*1.2, col=symColor)
  }
  ## Repeat for farouts
  if(is.null(z$farout) || length(z$farout) == 0)
    pick <- FALSE
  else
    pick <- (z$farout > z$censored & z$farout > z$estimated)
  if(any(pick)) {
    out <- z$farout[pick]
    points(rep(x, length(out)), out, pch=4, cex=par("cex")*1.2)
  }
  if(is.null(z$farout) || length(z$farout) == 0)
    pick <- FALSE
  else
    pick <- (z$farout > z$censored & z$farout < z$estimated)
  if(any(pick)) {
    out <- z$farout[pick]
    points(rep(x, length(out)), out, pch=4, cex=par("cex")*1.2, col="gray50")
  }
  
  ## Draw a detction limit line if possible and requested
  detlim <- max(z$censored, z$estimated)
  if(draw.RL && detlim > par("usr")[3L]) {
    if(detlim == z$censored) # Erase any line drawn at the RL
      segments(x - 0.5 * width, detlim, x + 0.5 * width, detlim,
               lwd=lineWt("standard"), col="white")
    segments(x - 0.75 * width, detlim, x + 0.75 * width, detlim,
             lwd=lineWt("standard"), col="black", lty="dashed")
  }
  ## Do we need to warn users about unusual estimated values?
  if(!is.null(z$critical) && z$critical > z$estimated)
    segments(x - 0.75 * width, z$critical, x + 0.75 * width, z$critical,
             lwd=lineWt("bold"), col="red")
  ## OK, we are done
  z$x <- x
  z$width <- width
  invisible(z)
}

