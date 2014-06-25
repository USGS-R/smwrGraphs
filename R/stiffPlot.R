# Beginning of the stiff function code.
#
#    2003Jan30 DLLorenz Fixed ylabels bug (forced to character)
#                       and added stop for missing data.
#    2007Oct16 DLLorenz Format file
#    2012Nov26 DLLorenz Conversion to R and correspond to those standards
#    2013Apr09 DLLorenz Added setGD
#

stiffPlot <- function(cations, anions, # the data matrices
                      Stiff=list(fill="gray50", outline="black",
                        height=2/3, bar="black"), # the plot
                      yaxis.orient="table", yaxis.order="none",
                      xaxis.range=c(NA, NA),
                      ylabels="Auto", xlabels=7,
                      catlabels="Auto", anlabels="Auto",
                      xtitle = "Milliequivalents per liter",
                      ytitle = "", caption="", # labels and titles
                      margin=c(NA, NA, NA, NA), ...) {
  ## Arguments:
  ##  cations matrix or DF the cations (left-hand data)
  ##  anions matrix or DF the cations (right-hand data)
  ##  Stiff list of plot controls
  ##  ylabels car vector the labels for the Stiff diagrams
  ##  xlabels single numeric the approxoimate number of labels
  ##  catlabels, anlabels character to match num cols the labels for the data
  ##  xtitle char. x-axis title
  ##  ytitle char. y-axis title
  ##  rest as usual.
  ##
  if(dev.cur() == 1)
    setGD("StiffPlot")
  ## Labels for explanation
  if(catlabels[1L] == "Auto") {
    catlabels <- colnames(cations)
    if(is.null(catlabels))
      catlabels <- paste("cat", seq(ncol(cations)))
  }
  if(anlabels[1L] == "Auto") {
    anlabels <- colnames(anions)
    if(is.null(anlabels))
      anlabels <- paste("an", seq(ncol(anions)))
  }
   ## Set Stiff defaults
  Stiff <- setDefaults(Stiff, fill="gray80", outline="black", height=2/3,
                       bar="black")
  ## Get ylabels (yaxis labels for the Stiff diagrams)
  if(ylabels[1L] == "Auto") {
    ylabels <- rownames(cations)
    if(is.null(ylabels))
      ylabels <- format(seq(nrow(cations)))
  }
  ## Set plot limits
  xmax <- max(cations, anions, na.rm=TRUE)
  if(any(is.na(xaxis.range)))
    xax <- linearPretty(c(-xmax, xmax), labels=xlabels)
  else
    xax <- linearPretty(xaxis.range, hard=TRUE, labels=xlabels)
  yaxis.orient <- match.arg(yaxis.orient, c("table", "grid"))
  if(length(yaxis.order) == 1L)
    yaxis.order <- match.arg(yaxis.order, c("none", "ascending", "descending"))
  yax <- namePretty(ylabels, orientation=yaxis.orient, order=yaxis.order,
                    label.abbr=FALSE)
  ylev <- yax$labels
  y <- numericData(ylabels, ylev)
  margin <- setMargin(margin, yax=yax)
  par(mar=margin$margin)
  ## Plot is set up with extended ranges (default)
  plot(0, 1, ylim = yax$range, xlim=xax$range, type = "n", axes = F,
       xlab="", ylab="")
  ## Set up for addStiff
  N <- length(y)
  x <- rep(0.0, N)
  pin <- par("pin")
  width <- pin[1L]
  height <- pin[2L]/N
  xrange <- par("usr")[c(1L, 2L)]
  ## Do not draw bar in addStiff, recovere after call
  Sbar <- Stiff$bar
  Stiff$bar <- "none"
  retval <- addStiff(x, y, width, height, cations, anions, Stiff,
                     xaxis.range=xrange)
  Stiff$bar <- Sbar
  retval$explan$stiff$catlabels <- catlabels
  retval$explan$stiff$anlabels <- anlabels
  retval$explan$stiff$Stiff <- Stiff
  ## Axis labels, etc
  ## Need to modify x-axis labels to represent only positive value
  xax$labels <- sub("-", "", xax$labels)
  if(Stiff$bar != "none")
    abline(v=0, col=Stiff$bar, lwd=stdWt())
  renderY(yax, lefttitle=ytitle, left=list(ticks = FALSE, labels = TRUE,
                                   grid = FALSE, finegrid = FALSE,
                                   extend=FALSE),
          right=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
            finegrid = FALSE))
  renderX(xax, bottitle=xtitle, bottom=list(ticks = TRUE, labels = TRUE,
                                  grid = FALSE, finegrid = FALSE,
                                  extend = TRUE),
          top=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
            finegrid = FALSE), caption=caption)
  ## Need to complete the info needed in ret
  invisible(list(cations=retval$cations, anions=retval$anions,
                 yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.lev=ylev,
                 xaxis.log=FALSE, explanation=retval$explan, margin=margin,
                 yax=yax, xax=xax))
}

addStiff <- function(x, y, width, height,
                     cations, anions, # the data matrices
                     Stiff=list(fill="gray50", outline="black",
                       height=2/3, bar="black"), # the plot
                     xaxis.range=c(NA, NA),
                     catlabels="", anlabels="",
                     current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                       xaxis.log=FALSE)) {
  ## Arguments:
  ##  x x-coordinate data
  ##  y y-coordinate data
  ##  width the width of the Stiff diagram area in inches
  ##  height the height of the Stiff diagram area in inches
  ##  Stiff control for the diagram
  ##  xaxis.range set the range (corresponding to the width of the area
  ##  catlabels and anlabels, the labels for the Stiff diagram
  ##  current, plot charactersitics
  ##
  ## Convert the x, y data
  y <- numericData(y, lev=current$yaxis.lev)
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- numericData(x, lev=current$xaxis.lev) # Convert dates to consistent numeric
  x <- transData(x, current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  ## Process the cation/anion data
  ## Force cations and anions to be a matrix, assume x and y are length 1
  if(!is.matrix(cations))
    cations <- matrix(cations, nrow=1)
  if(!is.matrix(anions))
    anions <- matrix(anions, nrow=1)
  Miss <- apply(cbind(cations, anions), 1, function(x) any(is.na(x)))
  if(any(Miss)) {
    anions[Miss, ] <- NA
    cations[Miss, ] <- NA
  }
  Nobs <- length(Miss)
  if(Nobs != length(x))
    stop("length of x and y must match the number of observations in cations and anions")
  llcat <- ncol(cations)
  llan <- ncol(anions)
  ## Get factors to convert from inches to user units
  usr <- par("usr")
  pin <- par("pin")
  xin <- pin[1L]/(usr[2L] - usr[1L])
  yin <- pin[2L]/(usr[4L] - usr[3L])
  ## set xaxis.range if necessary
  xmax <- max(c(anions, cations), na.rm=T)
  if(any(is.na(xaxis.range))) {
    xmax <- max(pretty(c(0, xmax), 2))
    xaxis.range <- c(-xmax, xmax)
  }
  ## Get the base Y-values of the polygons
  yvalues <- c((0:(llcat-1))/(llcat-1),((llan-1):0)/(llan-1)) - 0.5
  yvalues <-  yvalues / yin * Stiff$height * height
  ybar <- c(-.5, .5) / yin * height
  ## reverse columns in anions so that they plot from bottom to top
  antmp <- anions[, seq(llan, by=-1), drop=FALSE]
  data <- cbind(-cations, antmp)/diff(xaxis.range) * width / xin
  xlabpos <- c(-width, width) / 2 / xin
  ## Set Stiff defaults
  Stiff <- setDefaults(Stiff, fill="gray80", outline="black", height=2/3,
                       bar="black")
  for(i in seq(Nobs)) {# plot the data!
    polygon(data[i,, drop=TRUE] + x[i], yvalues+y[i], col=Stiff$fill,
            border=Stiff$outline, lwd=stdWt())
    if(Stiff$bar != "none")
      segments(x[i], ybar[1L]+y[i], y1=ybar[2L]+y[i], col=Stiff$bar, lwd=stdWt())
    if(catlabels[1L] != "") { # Assume that labels should be drawn
      text(rep(xlabpos[1L], llcat) + x[i], yvalues[1:llcat] + y[i],
           labels=catlabels, adj=0, family="USGS")
      ## Reverse labels so the order is from bottom to top by column
      revanlab <- rev(anlabels)
      text(rep(xlabpos[2L], llan) + x[i], yvalues[(llcat+1):(llcat+llan)] + y[i],
           labels=revanlab, adj=1, family="USGS")
      ## Dotted lines connecting labels to polygon
      for(j in seq(llcat))
        segments(xlabpos[1L] + x[i] + strwidth(catlabels[j], family="USGS"),
                 yvalues[j] + y[i], data[i,j] + x[i],
                 lty="dotted", lwd=stdWt())
      for(j in 1:llan)
        segments(xlabpos[2L] + x[i] - strwidth(revanlab[j], family="USGS"),
                 yvalues[llcat+j] + y[i], data[i, llcat+j] + x[i],
                 lty="dotted", lwd=stdWt())
    }
  }
  ## Construct dummy data for explanation
  cats <- rep(c(.65, .5), length.out=llcat)
  ans <- rep(c(.65, .5), length.out=llan)
  explan <- list(stiff=list(x=0, y=1, width=2, height=height,
                   cations=cats, anions=ans, Stiff=Stiff, catlabels=catlabels,
                   anlabels=anlabels, xaxis.range=c(-1, 1)))
  invisible(list(cations=cations, anions=anions, explan=explan))
}
