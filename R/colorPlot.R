# Plot x,y data changing symbol color by a class or continuous variable
#
# Coding History:
#    2011Apr16 DLLorenz Original coding from xyPlot.
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2012Sep27 DLLorenz Made generic
#    2013Apr09 DLLorenz Added setGD 
#

setGeneric("colorPlot", function(x, y, color, Plot=list(),
                              yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.range=c(NA,NA),
                              xaxis.log=FALSE, xaxis.range=c(NA,NA),
                              ylabels=7, xlabels=7, xtitle="", ytitle="",
                              caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("colorPlot"))

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
  ## Arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
  ##   color - the colors or a class to set colors 
  ##   Plot - parameters of the plot
  ##     name="Auto" means derive class names from the argument color,
  ##       otherwise, must be a tagged list of color=name, ... (in which case
  ##       the color tag is not used).
  ##     color="Auto" means if color is double or dateLike create groups
  ##       # of classes, otherwise create unique colors, alternate values
  ##       are "Range" (treat like double), tagged list of group_name=color, ...,
  ##       "Discrete" valid only for numeric, or
  ##       "Index" valid only for integer or for specified colors
  ##     groups - number of classes to create from argument color if numeric
  ##     ramp - the prefix to a color ramp, heat, terrain, topo, cm, gray,
  ##       or one of the provided color ranges in this library
  ##   xaxis.log - log-transform the X axis
  ##   xaxis.range - set the range of the x-axis
  ##   yaxis.log - log-transform the Y axis
  ##   yaxis.rev - reverse the Y axis
  ##   yaxis.range - set the range of the y-axis
  ##   xlabels - an estimate of the number of labels wanted
  ##   ylabels - an estimate of the number of labels wanted
  ##     NOTE: either xlabels or ylabels can be a list of arguments to
  ##     linearPretty or logPretty to tweak output
  ##   xtitle - x-axis title
  ##   ytitle - y-axis title
  ##   caption - the figure caption
  ##   margin - the parameters of the margin
  ##
  ## create the plotting positions
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
      ColorN <- min(12, length(unique(color)))
      ColorBrk <- quantile(ColorRange, probs=seq(0, ColorN) / ColorN)
      color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
      ColorNdx <- rev(levels(color))
      ## Assign levels to name and colors to color
      Plot$name <- as.character(color)
      ColorVals <- ColorRamp(length(levels(color)))
      names(ColorVals) <- levels(color)
      Plot$color <- ColorVals[color]
    }
    else if(ColorNumeric && Plot$color == "Auto") {
      ColorBrk <- quantile(color, probs=seq(0, Plot$groups) / Plot$groups, na.rm=TRUE)
      color <- cut(color, breaks=ColorBrk, include.lowest=TRUE)
      ColorNdx <- rev(levels(color))
      ## Assign colors to name
      Plot$name <- as.character(color)
      ColorVals <- rainbow(Plot$groups, end=2/3)
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
    parms <- setMultiPlot(Plot, length(x), name="", what='points', type='solid',
                          width='standard', symbol='circle', filled=TRUE,
                          size=0.09, color='black', order=ColorNdx)
    points(x, y, type='p', pch=parms$current$pch, cex=parms$current$cex,
           col=parms$current$col, bg=parms$current$col)
    ## Fix color range to show only the min and max
    if(SetRange) {
      Plot$name <- rev(paste(signif(ColorRange, 3), c('(min)', '(max)'), sep=' '))
      Plot$color <- ColorVals[c(length(ColorVals), 1)]
      parms <- setMultiPlot(Plot, length(x), name="", what='points', type='solid',
                          width='standard', symbol='circle', filled=TRUE,
                          size=0.09, color='black')
    }
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
