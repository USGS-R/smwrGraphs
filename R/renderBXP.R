# USGS style boxplot support functions.
#
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
#

## Draw the boxplot
renderBoxPlot <- function(xtoplot, stats, Box, explan, expz, # from compute stats + Box
                          yaxis.log=FALSE, yrange=c(NA,NA), # y-axis controls
                          xrange=range(xtoplot) + c(-1, 1),
                          ylabels='Auto', xlabels='Auto', xlabels.rotate=FALSE, # labels
                          xtitle='', ytitle='',  caption='', # axis titles and caption
                          margin=c(NA,NA,NA,NA)) { # margin control
  ## Arguments:
  ##  xtoplot (numeric vector) the x-axis locations of the box
  ##  stats (tagged list) the modified output from boxplot
  ##  explan (tagged list) the proper explanation, passed in return object
  ##  expz (tagged list) the explanation of the boxplot 
  ##  remainder as standard
  ## margin is used to control set up of plot:
  ## The margin is computed  and ticks drawn for any y-axis that has
  ## an unset margin of NA (default).
  ## If any are set 0 or positive, then it is assumed that the plot is set up
  ## and tick are drawn, but no labels
  ## If any are set negative, then it is assumed the the plot is set up
  ## and ticks are not drawn. if the value is less than -100, that is treated
  ## like -0.
  ## Ticks are not drawn on nonnumeric x-axis to not conflict with whiskers
  ##
  ## Note that points are drawn in this function for n <= nobox, otherwise
  ## in the call to renderBXP
  ##
  ## Preliminaries
  xrange <- xrange
  if(dev.cur() == 1)
    setGD("BoxPlot")
  size.lab <- 0.7 # size of outlier symbols
  mindat <- stats$data.range[1]
  maxdat <- stats$data.range[2]
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
  if(xlabels[1] != "Auto") 
    parnames <- xlabels
  else
    parnames <- names(stats)
  nbox <- length(xtoplot)
  ## Set margins and controls
  if(xlabels.rotate) {
    botmar <- max(strwidth(parnames, units='inches', family='USGS'))/par('cin')[2] + 2.2
    if(is.na(margin[1]))
      margin[1] <- pmax(3.2, botmar)
  }
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  if(xlabels.rotate)
    bot$angle <- 90 # Use this logic
### I'm pretty sure something needs to be tweaked here or in the set up call
  if(is.numeric(xlabels)) # build the x-axis labels
    xax <- linearPretty(xrange, labels=xlabels)
  else {
    xax <- namePretty(parnames, orientation='grid', offset=1)
    bot$ticks <- top$ticks <- FALSE
  }
  xrange <- xax$range
  meanspacing <- diff(xrange)/(nbox + 1)
  par(mar=margin)
  ## set up the boxplot, use renderBXP
  par(lwd=stdWt()) # standard line weight
  plot(mean(xrange), mean(yrange), type='n', xlim=xrange, xaxs='i', axes=FALSE,
       ylim=yrange, yaxs='i', ylab="", xlab="")
  ux <- par('pin')[1]/diff(par('usr')[1:2]) # the inches per x-usr unit
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
      if(exists(".pdf_graph") && get(".pdf_graph"))
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
  if(xlabels[1] != "Auto") 
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
  explanation <- c(explan, expz, n='32')
  if(!Box$show.counts) { # remove that from the stuff to draw
    explanation$n <- NULL
    explanation$labels <- explanation$labels[-1]
    explanation$values <- explanation$values[-1]
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

## The workhorse function
renderBXP <- function(x, width, z, draw.RL=TRUE, fill="none") {
  ## Arguments:
  ##  x (numeric scalar) the x-axis location
  ##  width (numeric scalar) the width of the box
  ##  z (tagged list) the box info
  ##    for z, stats (y) is min, .25, .50, .75, and max
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
  if(y[4] < z$estimated) # draw the whole in gray
    polygon(c(xmd, xmd, xpd, xpd), c(y[2], y[4], y[4], y[2]),
            density=den, col=fill, border="gray50", lwd=lineWt('standard'))
  else # draw in black and over print if necessary
    polygon(c(xmd, xmd, xpd, xpd), c(y[2], y[4], y[4], y[2]),
            density=den, col=fill, border="black", lwd=lineWt('standard'))
  ## Overplot with lines if estimated
  if(y[2] < z$estimated) {
    segments(xmd, z$estimated, xmd, y[2], lwd=lineWt('standard'), col="gray50")
    segments(xmd, y[2], xpd, y[2], lwd=lineWt('standard'), col="gray50")
    segments(xpd, z$estimated, xpd, y[2], lwd=lineWt('standard'), col="gray50")
  }
  ## median
  if(y[3] < z$estimated) # gray for estimated
    segments(xmd, y[3], xpd, y[3], lwd=lineWt('standard'), col="gray50")
  else
    segments(xmd, y[3], xpd, y[3], lwd=lineWt('standard'), col="black")
  ## whiskers with end caps
  if(y[2] < z$estimated) {
    segments(x, y[1], x, y[2], lwd=lineWt('standard'), col="gray50")
    segments(x - 0.5 * width, y[1], x + 0.5 * width, y[1],
             lwd=lineWt('standard'), col="gray50")
  }
  else if(y[1] < z$estimated) {
    segments(x, y[1], x, z$estimated, lwd=lineWt('standard'), col="gray50")
    segments(x, z$estimated, x, y[2], lwd=lineWt('standard'), col="black")
    segments(x - 0.5 * width, y[1], x + 0.5 * width, y[1],
             lwd=lineWt('standard'), col="gray50")
  }
  else {
    segments(x, y[1], x, y[2], lwd=lineWt('standard'), col="black")
    if(!ck[1]) # do not end cap draw if censored 
      segments(x - 0.5 * width, y[1], x + 0.5 * width, y[1],
               lwd=lineWt('standard'), col="black")
  }
  if(y[5] < z$estimated) {
    segments(x, y[4], x, y[5], lwd=lineWt('standard'), col="gray50")
    segments(x - 0.5 * width, y[5], x + 0.5 * width, y[5],
             lwd=lineWt('standard'), col="gray50")
  }
  else if(y[4] < z$estimated) {
    segments(x, y[4], x, z$estimated, lwd=lineWt('standard'), col="gray50")
    segments(x, z$estimated, x, y[5], lwd=lineWt('standard'), col="black")
    segments(x - 0.5 * width, y[5], x + 0.5 * width, y[5],
             lwd=lineWt('standard'), col="black")
  }
  else {
    segments(x, y[4], x, y[5], lwd=lineWt('standard'), col="black")
    segments(x - 0.5 * width, y[5], x + 0.5 * width, y[5],
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
    if(exists(".pdf_graph") && get(".pdf_graph"))
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
    if(exists(".pdf_graph") && get(".pdf_graph"))
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
  if(draw.RL && detlim > par("usr")[3]) {
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

