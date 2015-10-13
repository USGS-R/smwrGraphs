#' @title Stiff Diagrams
#' 
#' @description Add a Stiff diagram to an existing graph or create a tabular presentation of
#'Stiff diagrams in a graph.
#' 
#' @details The units of the cation and anion data are generally in milli-equivalents
#'per liter.
#' 
#'The \code{Stiff} argument must be a tagged list with these components:
#'\describe{ \item{fill}{the name of the color to fill each Stiff diagram.
#'Must be a valid color name.} \item{outline}{the name of the color to draw
#'the outline or border for each Stiff diagram.  Must be a valid color name.}
#'\item{height}{the height of each Stiff diagram, proportional to the overall
#'height for each Stiff diagram.} \item{bar}{the color of the central bar. May
#'be "none" for no central bar.} }
#' 
#'The values for \code{axis.range} must be expressed as a negative value for the 
#'\code{cation} data, left-hand side of the diagram, and a positive value for 
#'\code{anion} data, right-hand side of the diagram.
#'
#' @aliases addStiff stiffPlot
#' @param x the x-coordinates to place the center of each Stiff diagram.
#' Missing values are permitted, but result in no Stiff diagram.
#' @param y the y-coordinates to place the center of each Stiff diagram.
#' Missing values are permitted, but result in no Stiff diagram.
#' @param width the width in inches of the Stiff diagrams.
#' @param height the height in inches of the Stiff diagrams.
#' @param cations a matrix of cation data. Each row corresponds to the
#' respective \code{x} and \code{y} value. Missing values are not permitted for
#' \code{addStiff}, but are permitted for \code{stiffPlot} and result in no
#' Stiff Diagram for that entry.
#' @param anions a matrix of anion data. Each row corresponds to the respective
#' \code{x} and \code{y} value. Missing values are not permitted for
#' \code{addStiff}, but are permitted for \code{stiffPlot} and result in no
#' Stiff Diagram for that entry.
#' @param Stiff a list describing the Stiff diagram. See \bold{Details}.
#' @param xaxis.range the range of the x-axis corresponding to \code{width} in
#' the call to \code{addStiff} or the range of the x-axis in the call to
#' \code{stiffPlot}. See \bold{Details}.
#' @param catlabels labels for the values of the cations. For \code{addStiff},
#' the labels are applied to each Stiff diagram and for \code{stiffPlot}, the
#' labels are stored and drawn on the explanation \code{addExplanation}.
#' @param anlabels labels for the values of the anions. For \code{addStiff}, the
#' labels are applied to each Stiff diagram and for \code{stiffPlot}, the
#' labels are stored and drawn on the explanation \code{addExplanation}.
#' @param current the current plotting parameters. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @param yaxis.orient orientation of the y-axis values, must be either "table"
#' or "grid." "Table" is sorted from top to bottom, "grid" is sorted from
#' bottom to top.
#' @param yaxis.order the order of the y-axis values, must be one of "none,"
#' "ascending," or "descending."
#' @param ylabels set up y-axis labels.
#' @param xlabels set up x-axis labels.
#' @param xtitle x-axis title (also called x-axis caption).
#' @param ytitle y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @param \dots not used, required for other methods.
#' @return Information about the graph.
#' @seealso \code{\link{addStiff}}, \code{\link{xyPlot}}
#' @references Hem J.D., 1989, Study and interpretation of the chemical
#' characteristics of natural water: U.S. Geological Survey Water-Supply Paper
#' 2254, 263 p.
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of stiffPlot:
#' vignette(topic="PiperPlot", package="smwrGraphs")
#' # See for examples of addStiff:
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export stiffPlot
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
	#    2003Jan30 DLLorenz Fixed ylabels bug (forced to character)
	#                       and added stop for missing data.
	#    2007Oct16 DLLorenz Format file
	#    2012Nov26 DLLorenz Conversion to R and correspond to those standards
	#    2013Apr09 DLLorenz Added setGD
	#    2014Jun26 DLLorenz Converted to roxygen
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
  retval <- list(cations=retval$cations, anions=retval$anions,
  							 yaxis.log=FALSE, yaxis.rev=FALSE, yaxis.lev=ylev,
  							 xaxis.log=FALSE, explanation=retval$explan, margin=margin,
  							 yax=yax, xax=xax)
  # The width and xaxis.range must be set from the data
  ck <- max(strwidth(c(retval$explanation$stiff$anlabels, retval$explanation$stiff$catlabels),
  									 family="USGS", units="inches")) - .25
  if(ck > 0) {
  	retval$explanation$stiff$width <- 2 + 2*ck
  	retval$explanation$stiff$xaxis.range <- retval$explanation$stiff$xaxis.range * (1 + ck)
  }
  invisible(retval)
}



#' @rdname stiffPlot
#' @export addStiff
addStiff <- function(x, y, width, height,
                     cations, anions, # the data matrices
                     Stiff=list(fill="gray50", outline="black",
                       height=2/3, bar="black"), # the plot
                     xaxis.range=c(NA, NA),
                     catlabels="", anlabels="",
                     current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                       xaxis.log=FALSE)) {
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
