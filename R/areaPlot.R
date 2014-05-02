# Create an area plot
#
# Coding history:
#    2011Jun14 DLLorenz Initial coding
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Tweaks for package
#    2013Apr09 DLLorenz Added setGD 
#

areaPlot <- function(x, y, # data specs
                     Areas=list(name="Auto", fillDir="between", base="Auto",
                       lineColor="black", fillColors="pastel"), # Area controls
                     yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                     xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
                     ylabels=7, xlabels="Auto", # axis labels
                     xtitle="",
                     ytitle="", # axis titles, blank by default
                     caption="",# caption
                     margin=c(NA, NA, NA, NA)) { # margin control
  ## Arguments:
  ##  x (numeric vector) x-axis coordinates in increasing order
  ##  y (numeric matrix) y-axis coordinates
  ##  Areas (tagged list) name, "Auto" (derive names from y colnames or
  ##    names of areas; fillDir, "between" or "under"; lineColor, color
  ##    of lines; fillColor, either a name of a color sequence generator or
  ##    color names for each area.
  ##
  ## Set defaults for Areas
	Areas <- setDefaults(Areas, name="Auto", fillDir="between", 
											 base="Auto", lineColor="black",
											 fillColor="pastel")
  ## Set up plot (either xy or time)
  if(dev.cur() == 1)
    setGD("AreaPlot")
  y <- as.matrix(y) # Force to matrix if data.frame, for example
  xRng <- range(x, na.rm=TRUE)
  yRng <- range(y, na.rm=TRUE)
  ## Adjust y range if needed
  if(Areas$fillDir == "under" && Areas$base != "Auto")
    yRng[1] <- as.double(Areas$base)
  if(is.list(ylabels))
    yax <- c(list(data=yRng, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=FALSE), ylabels)
  else
    yax <- list(data=yRng, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=FALSE, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  yax <- yax$dax
  if(isDateLike(x)) { # Set up time axis
    if(any(is.na(xaxis.range)))
      tRng <- xRng
    else
      tRng <- xaxis.range
    xax <- datePretty(xaxis.range, major=xlabels)
    x <- numericData(x)
    xRng <- numericData(xRng)
  }
  else { # Regular x-axis
    if(!is.list(xlabels) && xlabels == "Auto")
      xlabels=7
    if(is.list(xlabels))
      xax <- c(list(data=xRng, axis.range=xaxis.range, axis.log=xaxis.log,
                    axis.rev=FALSE), xlabels)
    else
      xax <- list(data=xRng, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE, axis.labels=xlabels)
    xax <- do.call("setAxis", xax)
    xax <- xax$dax
  }
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  current=list(yaxis.log=yaxis.log, yaxis.rev=FALSE, xaxis.log=xaxis.log)
  ##
  plot(xRng, yRng, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  ## Set up to add the areas
  nAreas <- ncol(y)
  if(Areas$fillDir == "between")
    nAreas <- nAreas - 1
  if(Areas$name[1] == "Auto") { # Generate names
    if(Areas$fillDir == "between")
      Areas$name <- paste(colnames(y)[-1], colnames(y)[-ncol(y)], sep='-')
    else # Must be under
      Areas$name <- colnames(y)
  }
  if(length(Areas$fillColors) == 1 && nAreas > 1) { # Generate colors
    ColorGen <- get(paste(Areas$fillColors, "colors", sep='.'))
    Areas$fillColors <- rev(ColorGen(nAreas)) # need to reverse becuase of order plotted
  }
  else # Use what's there
    Areas$fillColors <- rev(setColor(Areas$fillColors))
  ## Add areas--needs work on the explanation
  if(Areas$fillDir == "between") {
    for(i in seq(nAreas, 1)) {
      xToPlot <- c(x, rev(x))
      yToPlot <- c(y[, i+1L], rev(y[, i]))
      current <- addArea(xToPlot, yToPlot, Area=list(name=Areas$name[i],
                                             color=Areas$fillColors[i],
                                             outline=Areas$lineColor),
                         current=current)
    }
  }
  else { # Must be under
    if(Areas$base == "Auto")
      base <- yax$range[1L]
    else
      base <- Areas$base
    for(i in seq(nAreas, 1L)) {
      xToPlot <- c(x[1L], x, x[length(x)])
      yToPlot <- c(base, y[,i], base)
      current <- addArea(xToPlot, yToPlot, Area=list(name=Areas$name[i],
                                             color=Areas$fillColors[i],
                                             outline=Areas$lineColor),
                         current=current)
    }
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  current$margin <- margin
  ## update x and y with the input
  current$x <- x
  current$y <- y
  current$yax <- yax
  current$xax <- xax
  invisible(current)  
}
