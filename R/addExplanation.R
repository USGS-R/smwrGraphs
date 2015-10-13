#' @title Add Explanation
#' 
#' @description Adds an explanation to the current graph or to a new graph.
#' 
#' @details The value for \code{where} must be one of "ul," "ur," "ll," "lr," 
#'"cl," "cr," "uc," "lc,"
#'"cc," or "new." If "new," then the explanation is placed in a new graph,
#'otherwise, the first letter is an abbreviation for upper, lower, or center
#'and the second letter is an abbreviation for left, right, or center. the
#'explanation for a box plot, stiff diagram, or contour plot must be placed 
#'in a new graph. If \code{box.off} is \code{TRUE}, then the explanation abuts
#'the axes, otherwise it is placed slightly inset so that the text does not
#'interfere with the ticks.
#'
#' In most cases, \code{line.length} does not need to be changed. In some cases,
#'such as mass produced figures that will not be modified by an illustrator, the
#'\code{line.length} can be increased to show full dashes if dashed lines are
#'drawn. In gneralt, the illustrator should create dashed lines rather than drawing
#'them in the graph.
#' 
#' @param what a specialized object for an explanation, from the output from
#'calls to the plotting functions
#' @param where a description of where to put the explanation, see
#'\bold{Details}.
#' @param title the title of the explanation.
#' @param box.off logical, if \code{TRUE}, then box off the explanation with a
#'blank background and black box, otherwise the background is not blanked and no
#'bounding box is drawn.
#' @param margin the margin for a new graph
#' @param line.length the relative length of lines drawn in the explanation, see
#'\bold{Details}.
#' @return Nothing is returned.
#' @note The call to \code{addExplanation} should be the last in any sequence of
#'calls to construct a figure becuase it can alter some graphical parameters.\cr
#'Box plot explanations require fairly large graph areas because of the
#'detail required for some types. In general, a graph about 4.5 inches high is
#'needed for the Tukey type and 4 inches for other types and widths of 2.5 and
#'2 inches respectively. The sizes are smaller for the font type of "USGS."
#'If the graph area is smaller than required for the box plot explanation, then 
#'either a modified explanation is created or a warning is printed and the 
#'explanation may be unreadable.
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' setGD()
#' AA.pl <- xyPlot(X, Y, Plot=list(name="Random Points"))
#' addExplanation(AA.pl, where='ul')
#' # For more details of addExplanation see
#' vignette(topic="BoxPlots", package="smwrGraphs")
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' vignette(topic="GraphSetup", package="smwrGraphs")
#' vignette(topic="LineScatter", package="smwrGraphs")
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' demo(topic="Coplot-complexScatterPlot", package="smwrGraphs")
#' demo(topic="FlowDur-Measurements", package="smwrGraphs")
#' demo(topic="PiperScript", package="smwrGraphs")
#' demo(topic="RightAxisExample", package="smwrGraphs")
#' }
#' @export addExplanation
addExplanation <- function(what, where="new", 
													 title=expression(bold(EXPLANATION)),
													 box.off=where != "new",
                           margin=rep(0,4), line.length=2) {
	# Coding history:
	#    2008Jun12 DLLorenz Original Coding and start of revisions
	#    2010Nov30 DLLorenz Modified for R
	#    2011Jun17 DLLorenz Added shaded areas
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Feb27 DLLorenz Begin update to new pub standards for boxplots
	#    2012Sep18 DLLorenz Added long integers
	#    2012Oct26 DLLorenz Fill for box plot and other fixes
	#    2012Nov11 DLLorenz Fix for type == "none"
	#    2012Nov30 DLLorenz Begin tweaks for final/add stiff
	#    2013Feb13 DLLorenz Tweaks for box plot
	#    2013Aug19 DLLorenz Bug fix for warning tests.
	#    2014Apr18 DLLorenz Added boxing option, and fixed title
	#    2014Jun25 DLLorenz Converted to roxygen
	#    2015Jan07 DLLorenz Added makeWideBox function to reconfigure boxplot explanation
	#                       if not quite tall enough but, wide enough
  ##
  if(!is.null(what$explanation))
    what <- what$explanation # extract from list created by calls
  if(is.element("z", names(what))) { # must be a boxplot
  	makeWideBox <- function(expl, code) {
  		# code is short for type, 1 is simple/truncated, 2 is extended, 3 is tukey
  		## is the first line Number of values?
  		Ioff <- as.integer(as.character(expl$labels[[1]]) == "bold(\"Number of values\")")
  		if(code == 1L) { # adjust 50th percentile 
  			expl$labels[[3L + Ioff]] <- list(expression(bold("50th percentile (median)")))
  		} else if(code == 2L) {
  			expl$labels[[1L + Ioff]] <- 
  				list(expression(bold("Individual value above the 90th percentile")))
  			expl$labels[[4L + Ioff]] <- list(expression(bold("50th percentile (median)")))
  			expl$labels[[7L + Ioff]] <- 
  				list(expression(bold("Individual value below the 10th percentile")))
  			## Also need to tweak the upper outside value
  			expl$z$out[2L] <- 3.5
  			expl$values[1L + Ioff] <- 3.5
  		} else { # Can only be 3
  			expl$labels[[1L + Ioff]] <- 
  				list(expression(bold("Largest value within 1.5 times interquartile")),
  						 expression(bold("range above 75th percentile")))
  			expl$labels[[3L + Ioff]] <- list("50th percentile (median)")
  			expl$labels[[5L + Ioff]] <- 
  				list(expression(bold("Smallest value within 1.5 times interquartile")),
  						 expression(bold("range below 25th percentile")))
  			expl$labels[[6L + Ioff]] <- 
  				list(expression(paste(bold("Outside value"), symbol("\276"), "Value is > 1.5 and < 3  times the",  sep = "")),
  						 "  interquartile range beyond either end of box")
  			expl$labels[[7L + Ioff]] <- 
  				list(expression(paste(bold("Far-out value"), symbol("\276"), "Value ", is >= 3,  " times the",  sep = "")),
  						 "  interquartile range beyond either end of box")
  		}
  		return(expl)
  	}
    if(where != "new")
      stop('The where argument for a box plot explanation  must be "new"')
    par(lwd=stdWt(), mar=margin)
    fin <- par("fin")
    ## Some warnings and set size, note goofy logic for fonts sizes
  	csi <- par("csi")
  	if(csi > .25)
  		stop("Font size too large for box plot explanation") # PPTs
  	minH <- csi*15 + 0.5
  	code <- 1L
  	if(!is.null(what$z$out)) { # Add a bit for tukey and extended
  		minH <- minH + 2 - csi*7.5
  		code <- 2L
  	}
  	if(!is.null(what$z$farout)) {# Add a bit for tukey, when csi is .2
  		minH <- minH + csi*15 - 2
  		code <- 3L
  	}
  	# Increase by a bit for censored boxplots
  	if(!is.null(what$x$censored))
  		minH <- minH + .1 + round(csi, 1)
    minW <- !is.null(what$z$out) * 0.5 + !is.null(what$z$farout) + round(7 * csi, 1)
  	wideW <- !is.null(what$z$out) + !is.null(what$z$farout) * round(csi, 1) * 2 + round(7.5 * csi, 1)
  	setWide <- FALSE
    if(fin[2L] < (minH - 0.001)) {
    	if(fin[1L] > wideW) {
    		# make wide
    		what <- makeWideBox(what, code)
    		setWide <- TRUE
    	} else
    		warning("Explanation for box plot should be at least ", 
    						minH, " inches high")
    }
    ## The width is set based on the character height (go figure)
    if(fin[1L] < (minW - 0.001))
      warning("Explanation for box plot should be at least ", 
              minW," inches wide")
    ## Make boxplot height look OK if longer than minH inches
    ymin <- pmin(-4.5, 5 - (fin[2L]/minH)*9.5 )
    xmax <- fin[1L] # set up for x-axis units in inches
    plot(.5,0, axes=FALSE, type="n", xlab="", ylab="",  ylim=c(ymin,5),
         xlim=c(0, xmax))
    renderBXP(what$width *.5 + 0.02, width=what$width, what$z,
              fill=what$fill)
    ## Print the explanations
    par(adj=0.5)
    text(xmax/2, 5.0, title, family="USGS")
    if(!is.null(what$n)) 
      text(what$width *.5 + 0.02, what$values[1], what$n, family="USGS")
    par(adj=0)
    ## Must print labels individually because it is a list and also to account
    ##  for a complicated structure
    lineht <- strheight("M\nM") - strheight("M")
    for(i in seq(along=what$values)) {
      if(is.list(what$labels[[i]]))
        for(j in seq(along=what$labels[[i]]))
          text(what$width + 0.07, what$values[i] - (j - 1L) * lineht,
               what$labels[[i]][[j]], family="USGS")
      else
        text(what$width + 0.07, what$values[i], what$labels[[i]],
             family="USGS")
    }
    if(!is.null(what$comment))
      mtext(what$comment, side=1, family="USGS") # something to comment about
    if(!is.null(what$IQR)) { # add
      par(family="USGS")
      if(setWide) {
      	xfrom <- what$width + 0.07 + strwidth("50th percentile (median)") + 0.05
      } else
      	xfrom <- what$width + 0.07 + strwidth("50th percentile") + 0.05
      lines(xfrom + c(0, 0.1, 0.1, 0), what$z$stats[c(2L, 2L, 4L, 4L), 1L],
            col="black", lwd=lineWt("standard"))
      text(xfrom + .15, mean(what$z$stats[c(2L, 4L), 1L]), what$IQR,
           family="USGS")
    }
    ## Set up for additional explanation
    title <- ""
    box.off <- FALSE
    if(fin[1L] > 3.9)
      where <- "ur"
    else # Assume that either no explanation or at bottom
      where <- "lc"
  } # End of boxplot part
  else if(is.element("stiff", names(what))) { # Stiff diagram
    if(where != "new")
      stop('The where argument for a Stiff explanation  must be "new"')
    par(lwd=stdWt(), mar=margin)
    fin <- par("fin")
    ## Some warnings and set for other plots
    if(fin[2L] < what$stiff$height + .35 + 1.e-6)
      warning("Explanation for Stiff Diagram should be at least ",
              what$stiff$height + .35, " inches high")
    if(fin[1L] < what$stiff$width + .25 + 1.e-6)
      warning("Explanation for Stiff Diagram should be at least ",
              what$stiff$width + .25, " inches wide")
    ## Se the range and add the stiff
    ymax <- what$stiff$y + what$stiff$height/2 + 0.25
    ymin <- ymax - fin[2L]
    xmin <- what$stiff$x - what$stiff$width/2 - 0.125
    xmax <- xmin + fin[1L]
    par(usr=c(xmin, xmax, ymin, ymax))
    do.call(addStiff, what$stiff)
    ## Add the title
    par(adj=0.5)
    text((xmax+xmin)/2, ymax-.15, title, family="USGS")
    ## Set up for additional explanation
    title <- ""
    box.off <- FALSE
    if(fin[1L] > what$stiff$width + 1.5)
      where <- "ur"
    else # Assume that either no explanation or at bottom
      where <- "lc"
  } # End of Stiff
  else if(is.element("contour", names(what))) {
    if(where != "new")
      stop('The where argument for a filled explanation  must be "new"')
    ## Dummy call to plot to set up drawing environment
    plot(0,0, axes=FALSE, type="n", xlab="", ylab="", mar=c(0,0,0,0))
    par(lwd=what$contour$linewt, mar=margin)
    fin <- par("fin")
    breaks <- what$contour$breaks # need to separate for explanation
    yname <- .5 # adjustment for name
    ## Some warnings and set for other plots
    ytest <- max(what$contour$yvals) + .35
    if(fin[2L] < ytest + 1.e-6) {
      warning("Explanation for this plot should be at least ",
              ytest, " inches high; adjusting")
      yname <- .35
      div <- (ytest + .5)/fin[2L]
      what$contour$yvals <- what$contour$yvals/div
      if((length(breaks) %% 2L) == 1L) { # skip every other label
      	breaks[(seq(along=breaks) %% 2L) == 0L] <- NA
      } else if((length(breaks) %% 3L) == 1L) { # skip 2 label
      	breaks[(seq(along=breaks) %% 3L) != 1L] <- NA
      } else {
      	# must be multiple of 6--awkward, skip every other, but leave top
      	picks <- seq(2L, length(breaks) - 1L, by=2L)
      	breaks[picks] <- NA
      }
    }
    if(fin[1L] < 1 - 1.e-6)
      warning("Explanation for plot should be at least ",
              1, " inches wide")
    par(usr=c(0, fin[1L], 0, fin[2L]))
    ## Add the title
    par(adj=0.5)
    text(fin[1L]/2, fin[2L] - .15, title, family="USGS")
    ## Add fill, lines and text
    if(!is.null(what$contour$name) && what$contour$name != "") {
    	# Add the name of the contours to the explanation
    	what$contour$yvals <- what$contour$yvals + .2
    	yoff <- max(what$contour$yvals)
    	text(what$contour$xvals[2L] + .1, fin[2L] - yname,
    			 labels=what$contour$name, adj=0, family="USGS")
    }
    image(what$contour$xvals, fin[2L] - what$contour$yvals,
          what$contour$zvalues, col=what$contour$fillcol,
          add=TRUE, breaks=what$contour$breaks)
    if(what$contour$linecol != "none") {
    	lwd <- if(is.null(what$contour$linewt)) stdWt() else what$contour$linewt
      segments(what$contour$xvals[1L], fin[2L] - min(what$contour$yvals),
               what$contour$xvals[1L], fin[2L] - max(what$contour$yvals),
               col=what$contour$linecol, lwd=lwd)
      segments(what$contour$xvals[2L], fin[2L] - min(what$contour$yvals),
               what$contour$xvals[2L], fin[2L] - max(what$contour$yvals),
               col=what$contour$linecol, lwd=lwd)
      for(y in fin[2L] - what$contour$yvals)
        segments(what$contour$xvals[1L], y, what$contour$xvals[2L],
                 col=what$contour$linecol, lwd=lwd)
    }
    ## Works most consistently, at least from what I've seen
    breaks <- format(breaks, big.mark =",")
    breaks <- gsub(" ", "", breaks)
    breaks <- sub("NA", "", breaks)
    text(what$contour$xvals[2L] + .1, fin[2L] - what$contour$yvals,
         labels=breaks, adj=0, family="USGS")
    ## Adjust where for other stuff to add
    where <- "ur"
    title <- ""
    box.off <- FALSE
  }
  ## The "normal" explanation
  ## pos is x-y and corner info for the call to legend()
  if(where == "new")
    plot.new()
  pos <- switch(where,
                ul="topleft",
                ur="topright",
                ll="bottomleft",
                lr="bottomright",
                cl="left",
                cr="right",
                uc="top",
                lc="bottom",
                cc="center",
                new="center")
  ## Extract information or exit
  if(is.null(what$lines))
    return(invisible(what))
  par(family="USGS", mar=margin)
  Pch <- what$lines$pch
  Pch[what$lines$type %in% c("l", "h", "s")] <- NA
  Pch <- Pch[what$lines$type != "n"]
  Lty <- what$lines$lty
  Lty[what$lines$type == "p"] <- NA
  Lty <- Lty[what$lines$type != "n"]
  Fill <- what$areas$fill
  if(is.null(Fill)) # fill with NAs
    Fill <- rep(NA, along=what$lines$type)
  Fill <- Fill[what$lines$type != "n"]
  Border <- what$areas$border
  if(is.null(Border)) # fill with NAs
    Border <- rep(NA, along=what$lines$type)
  Border <- Border[what$lines$type != "n"]
  Text <- paste("   ", what$text$text, sep="")
  Text <- Text[what$lines$type != "n"]
  Etext <- expression()
  for(i in seq(along=Text))
  	Etext[i] <- as.expression(substitute(bold(name), list(name=Text[i])))
  Lwd <- what$lines$lwd
  Lwd <- Lwd[what$lines$type != "n"]
  Col <- what$lines$col
  Col <- Col[what$lines$type != "n"]
  Pex <- what$lines$cex
  Pex <- Pex[what$lines$type != "n"]
  ## Sort by areas, lines and points--areas assigned 1, lines 2, and points 3
  ## Need to not sort overlaid points and lines last!
  Seq <- (!is.na(Fill)) * 1L + (!is.na(Lty)) * 2L + 
  	(!is.na(Pch) & is.na(Lty) & is.na(Fill)) * 3L
  Seq <- order(Seq) # Use this for the order to place in explanation
  ## Only do legend if length(Seq) > 0
  if(length(Seq) > 0L) {
  	if(box.off) {
  		## Because this blanks out what is behind, no inset needed
  		legend(x=pos, legend=Etext[Seq], title=title, bty="o",
  					 fill=Fill[Seq], border=Border[Seq],
  					 pch=Pch[Seq], lty=Lty[Seq], lwd=Lwd[Seq],
  					 col=Col[Seq], pt.bg=Col[Seq], pt.cex=Pex[Seq],
  					 bg="white", box.lwd=frameWt(), box.col="black", y.intersp=1.1,
  					 seg.len=line.length)
  	} else {
  		## Calculate inset if title is not blank
  		title.blank <- !is.expression(title) && title == ""
  		if(!title.blank && substring(pos, 1L, 3L) == "top") {
  			inset=c(0.05, 0.12)/par("pin")
  		} else if(title.blank && substring(pos, 1L, 3L) == "top") {
  			inset=c(0.05, 0.)/par("pin")
  		} else
  			inset=0.05/par("pin")
  		legend(x=pos, legend=Etext[Seq], title=title, bty="n",
  					 fill=Fill[Seq], border=Border[Seq], inset=inset,
  					 pch=Pch[Seq], lty=Lty[Seq], lwd=Lwd[Seq],
  					 col=Col[Seq], pt.bg=Col[Seq], pt.cex=Pex[Seq], y.intersp=1.1,
  					 seg.len=line.length)
  		
  	}
  }
  invisible(what)
}
