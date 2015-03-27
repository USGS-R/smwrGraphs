#' Biplot
#' 
#' Produce a biplot, which is a plot of two different types of data on the same
#' graph.
#' 
#' The \code{xPlotLabels} and \code{yPlotLabels} arguments must be tagged lists
#' with these components: \describe{ \item{labels}{the labels. For
#' \code{xPlotLabels}, "rownames" means use the row names from \code{x} to
#' generate the labels. For \code{yPlotLabels}, "colnames" means use the column
#' names from \code{y} to generate the labels. Otherwise a character vector of
#' the labels.} \item{dir}{the direction the label text is placed from the
#' object.} \item{size}{the size of the label text.} \item{offset}{the distance
#' the labels is placed relative to the object.} \item{color}{the color of the
#' label text.} }
#' 
#' @param x a 2-column matrix of x- (column 1) and y- (column 2) coordinates for 
#' observations or equivalent.
#' @param y a 2-column matrix of x- (column 1) and y- (column 2) coordinates for
#' variables or equivalent.
#' @param separate.axes logical, if \code{TRUE}, then plot \code{x} and \code{y} data
#' on separate axes.
#' @param xPlot control information to plot the \code{x} data.  See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param yPlot control information to plot the \code{y} data.  See
#' \code{\link{setPlot}} for a description of the parameters. For \code{yPlot},
#' symbol can be "arrow" to indicate that an arrow is to be drawn from the
#' origin.
#' @param xPlotLabels control information for the \code{x} data labels. See
#' \bold{Details}.
#' @param yPlotLabels control information for the \code{y} data labels. See
#' \bold{Details}.
#' @param ylabels set y-axis labels for \code{x} data. See
#' \code{\link{linearPretty}} for details.
#' @param xlabels set x-axis labels for \code{x} data. See
#' \code{\link{linearPretty}} for details.
#' @param ylabels2 set y-axis labels for \code{y} data. See
#' \code{\link{linearPretty}} for details.
#' @param xlabels2 set x-axis labels for \code{y} data. See
#' \code{\link{linearPretty}} for details.
#' @param xtitle x-axis title (also called x-axis caption) for \code{x} data.
#' @param ytitle y-axis title (also called y-axis caption) for \code{x} data.
#' @param xtitle2 x-axis title (also called x-axis caption) for \code{y} data.
#' @param ytitle2 y-axis title (also called y-axis caption) for \code{y} data.
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param \dots not used, required for other methods.
#' @return Information about the graph
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{biPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{biPlot}}
#' @keywords hplot
#' @export
#' @method biPlot default
biPlot.default <- function(x, y, # data, both must be 2-column matrix
                           separate.axes=TRUE,
                           xPlot=list(name="observations", what='points',
                             type='solid', width='standard', symbol='circle',
                             filled=TRUE, size=0.05, color='black'),
                           yPlot=list(name='variables', width='color',
                             size=0.2, color='darkblue', symbol='arrow',
                             filled=FALSE), # plot controls
                           xPlotLabels=list(labels='rownames', dir='NE',
                             size=8, offset=0.35),
                           yPlotLabels=list(labels='colnames', dir='Auto',
                             size=8, offset=0.35, color='darkblue'), # object labels
                           ylabels=5,  xlabels=5,
                           ylabels2=5,  xlabels2=5, # axis labels
                           xtitle='', ytitle='', 
                           xtitle2='', ytitle2='', # axis titles
                           caption="", # caption
                           margin=c(NA, NA, NA, NA), ...) {
	# Coding History:
	#    2010Mar12 DLLorenz Original coding
	#    2011Apr15 DLLorenz Begin modifications for R, and modified arrows
	#    2011Aug03 DLLorenz Added axis labeling info to current
	#    2011Oct24 DLLorenz Tweaks for package
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun25 DLLorenz Converted to roxygen
  ## Set up plot
  if(dev.cur() == 1L)
    setGD("BiPlot")
  ## The only way to guarantee 'pretty axes' is to force 0,0 to center
  ## And must increase by a bit for force labeles to draw
  obsmax <- max(abs(x)) * 1.25
  varmax <- max(abs(y)) * 1.25
  if(!separate.axes)
    obsrange <- varrange <- c(-max(obsmax, varmax), max(obsmax, varmax))
  else {
    obsrange <- c(-obsmax, obsmax)
    varrange <- c(-varmax, varmax)
    ## make sure top and right margins are not missing
    if(is.na(margin[3L]))
      margin[3] <- 3.1
    if(is.na(margin[4L]))
      margin[4] <- 5.1
  }
  yax <- linearPretty(obsrange, labels=ylabels)
  xax <- linearPretty(obsrange, labels=xlabels)
  if(separate.axes) {
    yax2 <- linearPretty(varrange, labels=ylabels2)
    xax2 <- linearPretty(varrange, labels=xlabels2)
  }
  margin.control <- setMargin(margin, yax)
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  margin <- margin.control$margin
  ## These calls set up a plot area as in a call to plot
  par(mar=margin)
  par(pty='s') # set up square figure area
  plot(0, 0, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="") # set up the plotting range
  par(lwd=stdWt()) # standard line weight
  ## Now do the plotting
  ## if multiplot
  if(any(sapply(xPlot, length) > 1)) {
    parms <- setMultiPlot(xPlot, length(x), name="observations", what='points',
                          type='solid', width='standard', symbol='circle',
                          filled=TRUE, size=0.05, color='black')
    plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
    plot.info$x <- x[,1]
    plot.info$y <- x[,2]
    by(plot.info, plot.info$name, FUN=function(x) {
      points(x$x, x$y, type=x$what[1], lwd=x$lwd[1L], lty=x$lty[1L],
             pch=x$pch[1L], cex=x$cex[1L], col=x$col[1L], bg=x$col)
      return(1) })
    explan <- parms$Explan
  }
  else { # only a single feature
    xPlot <- setPlot(xPlot, name="observations", what='points',
                     type='solid', width='standard', symbol='circle',
                     filled=TRUE, size=0.05, color='black') # force defaults if not set
    explan <- setExplan(xPlot) # add info to set up explanation
    plotPars <- explan$current
    points(x[,1L], x[,2L], type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  }
  ## Label those points if requested
  if(!is.null(xPlotLabels$labels) && tolower(xPlotLabels$labels[1]) != 'none') { # OK do it
    if(tolower(xPlotLabels$labels[1L]) == 'rownames') {
      labels <- rownames(x)
      if(is.null(labels))
        labels <- as.character(seq(1L, nrow(x)))
    }
    else if(tolower(xPlotLabels$labels[1L]) == 'letters')
      labels <- rep(c(letters, LETTERS), length.out=nrow(x))
    else
      labels <- rep(xPlotLabels$labels, length.out=nrow(x))
    dir <- xPlotLabels$dir
    if(is.null(dir))
      dir <- 'NE'
    offset <- xPlotLabels$offset
    if(is.null(offset))
      offset <- 0.35
    size <- xPlotLabels$size
    if(is.null(size))
      size <- 8
    labelPoints(x[,1L], x[,2L], labels=labels, dir=dir, offset=offset, size=size)
  }
  ## Do the vectorsarrows!
  ## first save usr
  usr <- par('usr')
  if(separate.axes)
    par(usr=c(xax2$range, yax2$range))
  lwd <- yPlot$width
  if(is.null(lwd))
    lwd <- 'color'
  lwd <- lineWt(lwd)
  size <- yPlot$size
  if(is.null(size))
    size=0.2
  color <- yPlot$color
  if(is.null(color))
    color <- 'darkblue'
  what <- yPlot$symbol
  if(is.null(what))
    what <- 'arrow'
  if(what == 'arrow')
    arrows(0, 0, y[,1L], y[,2L], length=size, col=color, lwd=lwd)
  else { # draw points
    if(separate.axes)
      warning("Plotting points instead of arrows is valid only for common axes")
    yPlot$what <- "points"
    yPlot <- setPlot(yPlot)
    explan <- setExplan(yPlot) # add info to set up explanation
    plotPars <- explan$current
    points(y[,1L], y[,2L], type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  }
  ## need to figure out how to put arrows into an explanation
  if(separate.axes) {
    renderY(yax2, lefttitle='', left=list(), right=list(ticks=TRUE, labels=TRUE))
    renderX(xax2, bottitle='', bottom=list(), top=list(ticks=TRUE, labels=TRUE),
            caption='')
  }
  ## Label the arrows, or points
  if(!is.null(yPlotLabels$labels) && tolower(yPlotLabels$labels[1L]) != 'none') { # OK do it
    if(tolower(yPlotLabels$labels[1L]) == 'colnames') {
      labels <- rownames(y)
      if(is.null(labels))
        labels <- paste('Var', seq(nrow(y)), sep='-')
    }
    else if(tolower(yPlotLabels$labels[1L]) == 'letters')
      labels <- rep(c(LETTERS), length.out=nrow(x))
    else
      labels <- rep(yPlotLabels$labels, length.out=nrow(x))
    ## Determine direction of the arrow to set direction of the label
    dir <- yPlotLabels$dir
    if(is.null(dir) || tolower(dir[1L]) == 'auto') {
      ## generate an index to the direction, two are needed because N could be the
      ## smallest or the largest value
      dir <- as.integer(atan2(y[,1L], y[,2L])/pi*8 + 1)
      ## Fix negs
      dir[dir <= 0L] <- 16L + dir[dir <= 0L]
      dir <- c("N", "NE", "NE", "E", "E", "SE", "SE", "S", "S", "SW", "SW", "W", "W",
               "NW", "NW", "N")[dir]
    }
  }
  offset <- yPlotLabels$offset
  if(!is.null(offset))
    offset <- 0.75
  size <- yPlotLabels$size
  if(!is.null(size))
    size <- 8
  color <- yPlotLabels$color
  if(is.null(color))
    color <- 'darkblue'
  labelPoints(y[,1L], y[,2L], labels=labels, dir=dir, offset=offset, size=size,
              color=color)
  ## Restore usr
  par(usr=usr)
  box(lwd=frameWt())
  ## Label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible()
  return(list(x=x[,1L], y=x[,2L], yaxis.log=FALSE, yaxis.rev=FALSE,
              xaxis.log=FALSE, explanation=explan, margin=margin,
              yax=yax, xax=xax))
}
