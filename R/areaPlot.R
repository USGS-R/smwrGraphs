#' Shaded Area Plot
#' 
#' Produces a plot where the area between lines is filled with color.
#' 
#' @details The components of \code{Areas} are \code{name}, the name or names
#'to use to describe the areas in the explanation, the default "Auto" generates
#'names from the column names of \code{y}; \code{fillDir}, how to 
#'fill---must be either "between" or "under;" \code{base}, the base value when
#'\code{fillDir} is "under," can be "Auto" to draw to the x-axis or any numeric value;
#'\code{lineColor} specifies the color to draw the lines around each area, may be
#'"none" for no line drawn; \code{fillColors} specifies colors for each area, when only
#'a single area is drawn, then the value must be the name of a color, otherwise either a
#'vector of colornames or the prefix name of a function that generates a sequence of
#'colors. The prefix name is prepended to ".colors" for the name of the function. See the
#'documentation for these functions in the \bold{See Also} section.
#'
#' For linear axes, the range can be set to virtually any pair of values. For 
#'log axes, the choice of range is more resticted---for less than one log-cycle,
#'powers of whole numbers can be used; from 1 to about 3 log cycles, the choces 
#'should be powers of 3 or 10; and for more than 3 log cycles, the range sould be 
#'expressed only in powers of 10.
#'
#' @param x numeric x-axis coordinates in increasing order.
#' @param y a numeric matrix of y-axis coordinates.
#' @param Areas parameters controlling the areas. See \bold{Details}.
#' @param yaxis.log logical, if \code{TRUE}, then log transform y axis.
#' @param yaxis.range set y-axis range. See \bold{Details}.
#' @param xaxis.log logical, if \code{TRUE}, then log transform x axis.
#' @param xaxis.range set x-axis range. See \bold{Details}.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set up x-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the caption for the graph.
#' @param margin set up the plot area margins.
#' @return Information about the graph.
#' @seealso \code{\link{addArea}}, \code{\link{smwr.colors}}, \code{\link[grDevices]{heat.colors}}
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- seq(1, 9, by=.5)
#' Y <- runif(17) + runif(17)
#' # The default fillDir, between, requires at least a 2-column matrix
#' areaPlot(X, cbind(rep(0, 17),Y))
#' # For more details of areaPlot see
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export areaPlot
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
	# Coding history:
	#    2011Jun14 DLLorenz Initial coding
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Oct24 DLLorenz Tweaks for package
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun25 DLLorenz Converted to roxygen
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
	Areas$fillDir <- match.arg(Areas$fillDir, c("between", "under"))
  ## Adjust y range if needed
  if(Areas$fillDir == "under" && Areas$base != "Auto")
    yRng[1] <- as.double(Areas$base)
  if(is.list(ylabels))
    yax <- c(list(data=yRng, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=FALSE), ylabels)
  else
    yax <- list(data=yRng, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=FALSE, axis.labels=ylabels, extend.range=FALSE)
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
