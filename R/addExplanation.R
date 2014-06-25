# USGS explanation
#
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
#

addExplanation <- function(what, where="new", 
													 title=expression(bold(EXPLANATION)),
													 box.off=where != "new",
                           margin=rep(0,4)) {
  ## arguments
  ## what is either an object for an explanation, or the output from calls
  ## to the plotting functions See first executable statment
  ##
  ## what is either a list for the key() function or the list for a boxplot,
  ##   conceivably, it could be something else too.
  ##   If it is a list for key(), then it must contain these components:
  ##    text - a list with the description
  ##    lines - a list with these components, one element for each description
  ##      col, lwd, type, lty, and pch
  ##    rectangle - an optional list for shaded areas, if present, then
  ##      the sum of the elements of this list and lines must equal the number
  ##      of descriptions. This is not currently implemented.
  ## where is a description of where to put the explanation
  ##   if 'new' then create a new plot and center, otherwise
  ##   'ul' for upper-left corner, 'ur' for upper-right corner,
  ##   'll' for lower-left corner, 'lr' for lower-right corner,
  ##   'cl' for center left,       'cr' for center right,
  ##   'uc' for upper center,      'lc' for lower center, or
  ##   'cc' for center.
  ##
  if(!is.null(what$explanation))
    what <- what$explanation # extract from list created by calls
  if(is.element("z", names(what))) { # must be a boxplot
    if(where != "new")
      stop('The where argument for a box plot explanation  must be "new"')
    par(lwd=stdWt(), mar=margin)
    fin <- par("fin")
    ## Some warnings and set for other plots
    outy <- !is.null(what$z$out)  * 0.5 + !is.null(what$z$farout)
    twid <- round(7 * par("csi"), 1)
    bh <- twid + 2.1 + outy
    if(fin[2L] < bh + 1.e-6)
      warning("Explanation for box plot should be at least ", 
              bh, " inches high")
    ## The width is set based on the character height (go figure)
    if(fin[1L] < twid + outy + 1.e-6)
      warning("Explanation for box plot should be at least ", 
              twid + outy," inches wide")
    ## Make boxplot height look OK if longer than 3.5 inches
    ymin <- pmin(-4.5, 5 - (fin[2L]/bh)*9.5 )
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
      xfrom <- what$width + 0.07 + strwidth("75th percentile") + 0.05
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
    par(lwd=stdWt(), mar=margin)
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
    if(fin[1L] < 1 + 1.e-6)
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
      segments(what$contour$xvals[1L], fin[2L] - min(what$contour$yvals),
               what$contour$xvals[1L], fin[2L] - max(what$contour$yvals),
               col=what$contour$linecol)
      segments(what$contour$xvals[2L], fin[2L] - min(what$contour$yvals),
               what$contour$xvals[2L], fin[2L] - max(what$contour$yvals),
               col=what$contour$linecol)
      for(y in fin[2L] - what$contour$yvals)
        segments(what$contour$xvals[1L], y, what$contour$xvals[2L],
                 col=what$contour$linecol)
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
  Seq <- (!is.na(Fill)) * 1L + (!is.na(Lty)) * 2L + (!is.na(Pch) & is.na(Lty)) *
    3L
  Seq <- order(Seq) # Use this for the order to place in explanation
  ## Only do legend if length(Seq) > 0
  if(length(Seq) > 0L) {
  	if(box.off) {
  		legend(x=pos, legend=Etext[Seq], title=title, bty="o",
  					 fill=Fill[Seq], border=Border[Seq],
  					 pch=Pch[Seq], lty=Lty[Seq], lwd=Lwd[Seq],
  					 col=Col[Seq], pt.bg=Col[Seq], pt.cex=Pex[Seq],
  					 bg="white", box.lwd=frameWt(), box.col="black", y.intersp=1.1)
  	} else
  		legend(x=pos, legend=Etext[Seq], title=title, bty="n",
  					 fill=Fill[Seq], border=Border[Seq],
  					 pch=Pch[Seq], lty=Lty[Seq], lwd=Lwd[Seq],
  					 col=Col[Seq], pt.bg=Col[Seq], pt.cex=Pex[Seq], y.intersp=1.1)
  }
  invisible(what)
}
