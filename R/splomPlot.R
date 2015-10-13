#' Scatter Plot Matrix
#' 
#' Produce a matrix of scatter plots
#' 
#' \code{Panel} may be a tagged list, with any one of these options:\cr
#' loess=span, where span is the span argument to loess.smooth;\cr line=opt,
#' where opt='slr' for simple linear regression, or 'loc' for line of organic
#' correlation, or '1:1' for the 1:1 line.\cr The format of the lines is taken 
#' from \code{Plot}.
#' 
#' \code{Panel} may also be a function with 3
#' arguments, x, y, and current, that adds to the plot and returns updated plot
#' information. The function is called for each individual plot.
#' 
#' @param x the data to plot, must be either a matrix or a data frame.
#' @param layout the output from \code{setSplom}
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param Panel a list or a function, see \bold{Details}.
#' @param axis.log log-transform the x and y axis for all plots.
#' @param axis.range set x- and y-axis ranges for all plots.
#' @param labels set the number of labels for both the x and y axes. See
#' \code{\link{linearPretty}} for details.
#' @param caption the figure caption.
#' @return Information about the graph.
#' @note A call must be made to \code{setPage} and to \code{setSplom} to set up
#' the graphics environment before calling \code{splomPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{setSplom}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of splomPlot:
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @export splomPlot
splomPlot <- function(x, layout, # data and layout info
                      Plot=list(name="", what='points', type='solid',
                        width='standard', symbol='circle', filled=TRUE,
                        size=0.05, color='black'), # Plot controls
                      Panel=list(), # Panel (individual plots) controls
                      axis.log=FALSE, axis.range=c(NA,NA),# axis controls
                      labels=5, # labels
                      # axis titles always derived from column names in x
                      caption='') { # caption 
	# Coding History:
	#    2011Aug01 DLLorenz Original coding.
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Mar12 DLLorenz Bug fixes
	#    2013Apr09 DLLorenz Added setGD 
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  ## Panel Details:
  ##   Panel may be a tagged list, with any of these options:
  ##     loess=span, where span is the span argument to loess.smooth;
  ##     line=opt, where opt='slr' for simple linear regression, or
  ##       'loc' for line of organic correlation, or '1:1' for the 1:1 line.
  ##     The format of the lines is taken from Plot,
  ##   Panel may also be a function with 3 arguments, x, y, and current, that
  ##     adds to the plot. That function is called for each individual
  ##     plot.
  ## Set up defaults, etc.
  yaxis.rev <- FALSE
  if(dev.cur() == 1)
    setGD("SPLOM")
  Plot <- setPlot(Plot, name="", what='points', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.05, color='black')
  if(layout$touching) # Set up extended axis and labels, otherwise OK as is
    labels <- list(labels=labels, extend.pct=5, extend.range=FALSE)
  titles <- colnames(x)
  if(length(titles) != layout$num.variables)
    stop("Number of columns in x does not match those set up in layout")
  ## Two versions of splom:
  if(layout$show.all) {
    grid.no <- 0
    for(i in seq(layout$num.variables)) { # all rows
      for(j in seq(layout$num.variables)) { # all cols
        grid.no <- grid.no + 1
        marg <- setGraph(grid.no, layout)
        if(i == j) { # Just draw the text
          par(mar=marg)
          plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab='', ylab='')
          text(0.5, 0.5, labels=titles[i], family='USGS', cex=1.5)
        }
        else {
          if(i == 1 && j == layout$num.variables)
            cap <- caption
          else
            cap <- ''
          AA <- xyPlot(x[,j], x[,i], Plot=Plot,
                       yaxis.log=axis.log, yaxis.range=axis.range,
                       xaxis.log=axis.log, xaxis.range=axis.range,
                       ylabels=labels, xlabels=labels,
                       xtitle='', ytitle='',
                       caption=cap, margin=marg)
          if(i == 1)
            addAxisLabels('top', AA)
          if(j == layout$num.variables)
            addAxisLabels('right', AA)
          ## Add Panel request
          if(is.list(Panel)) {
            draw.line <- Panel$line
            PPlot <- Plot
            PPlot$what='lines'
            if(!is.null(draw.line)) {
              if(draw.line == 'slr') {
                PPlot$name <- "Regression line"
                ## For this and loc, the coefs were based on log10, adding option
                ##  has no effect if log axes were not set
                AA <- refLine(coefficients=lsfit(AA$x, AA$y)$coef, Plot=PPlot,
                              current=AA, log10=TRUE)
              }
              else if(draw.line == '1:1') {
                PPlot$name <- "1:1 line"
                AA <- refLine(coefficients=c(0, 1), Plot=PPlot,
                              current=AA)
              }
              else if(draw.line == 'loc') {
                PPlot$name <- "Line of Organic Correlation"
                slope <- sd(AA$y) / sd(AA$x)
                if(cor(AA$x, AA$y) < 0)
                  slope <- -slope
                icept <- mean(AA$y) - slope * mean(AA$x)
                AA <- refLine(coefficients=c(icept, slope), Plot=PPlot,
                              current=AA, log10=TRUE)
              }
            } # End of draw a line
            span <- Panel$loess
            if(!is.null(span)) {
              PPlot$name <- "Loess Smooth line"
              xy <- loess.smooth(AA$x, AA$y, span=span)
              if(axis.log)
                AA <- addXY(10^xy$x, 10^xy$y, Plot=PPlot, current=AA)
              else
                AA <- addXY(xy$x, xy$y, Plot=PPlot, current=AA)
            }
          } # End of panel as list
          else if(is.function(Panel))
            AA <- Panel(AA$x, AA$y, AA)
          else
            stop("Panel must be either a list or a function")
        } # End of else (plot the data)
      } # End of for j
    } # End of for i
  }
  else {
    xtitle=''
    for(i in seq(2, layout$num.variables)) { # rows start at 2
      ytitle <- titles[i]
      for(j in seq(1, i - 1)) { # cols start at 1
        grid.no <- (i - 2)*(layout$num.variables - 1) + j
        marg <- setGraph(grid.no, layout)
        if(i == layout$num.variables)
          xtitle <- titles[j]
        if(j > 1)
          ytitle=''
        if(i == layout$num.variables && j == 1)
          cap <- caption
        else
          cap <- ''
        ## Plot the data
        AA <- xyPlot(x[,j], x[,i], Plot=Plot,
                     yaxis.log=axis.log, yaxis.range=axis.range,
                     xaxis.log=axis.log, xaxis.range=axis.range,
                     ylabels=labels, xlabels=labels,
                     xtitle=xtitle, ytitle=ytitle,
                     caption=cap, margin=marg)
        ## Add Panel request
        if(is.list(Panel)) {
          draw.line <- Panel$line
          PPlot <- Plot
          PPlot$what='lines'
          if(!is.null(draw.line)) {
            if(draw.line == 'slr') {
              PPlot$name <- "Regression line"
              ## For this and loc, the coefs were based on log10, adding option
              ##  has no effect if log axes were not set
              AA <- refLine(coefficients=lsfit(AA$x, AA$y)$coef, Plot=PPlot,
                            current=AA, log10=TRUE)
            }
            else if(draw.line == '1:1') {
              PPlot$name <- "1:1 line"
              AA <- refLine(coefficients=c(0, 1), Plot=PPlot,
                            current=AA)
            }
            else if(draw.line == 'loc') {
              PPlot$name <- "Line of Organic Correlation"
              slope <- sd(AA$y) / sd(AA$x)
              if(cor(AA$x, AA$y) < 0)
                slope <- -slope
              icept <- mean(AA$y) - slope * mean(AA$x)
              AA <- refLine(coefficients=c(icept, slope), Plot=PPlot,
                            current=AA, log10=TRUE)
            }
          } # End of draw a line
          span <- Panel$loess
          if(!is.null(span)) {
            PPlot$name <- "Loess Smooth line"
            xy <- loess.smooth(AA$x, AA$y, span=span)
            if(axis.log)
              AA <- addXY(10^xy$x, 10^xy$y, Plot=PPlot, current=AA)
            else
              AA <- addXY(xy$x, xy$y, Plot=PPlot, current=AA)
          }
        } # End of panel as list
        else if(is.function(Panel))
          AA <- Panel(AA$x, AA$y, AA)
        else
          stop("Panel must be either a list or a function")
      } # End of for j
    } # End of for i
  } # End of show lower tri
  invisible(AA)
}
