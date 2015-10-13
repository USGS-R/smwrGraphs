#' Errors Bars
#' 
#' Add error bars to a graph.
#' 
#' The \code{Bars} argument must be a tagged list with these components:
#' \describe{ \item{name}{a name describing the data; used in the explanation.}
#' \item{cap}{the width of each cap on the error bar.} \item{width}{the width
#' of the lines drawn for the error bars.} \item{color}{the name of the color
#' to draw the error bars.} }
#' 
#' @param x the x-coordinate data. Missing values are permitted but result in
#' no bar.
#' @param yup the upper limit of the error bar. Missing values are permitted
#' but result in no bar.
#' @param ylo the lower limit of the error bar. Missing values are permitted
#' but result in no bar.
#' @param Bars parameters defining the characteristics of the error bars. See
#' \bold{Details}.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return The current plot information is returned invisibly.
#' @note The error bars are plotted on top of any current symbol. To plot the
#' symbol on top of the error bar, start with the argument
#' \code{Plot=list(what="none")} in the original call to \code{xyPlot} or
#' \code{timePlot} and then add the symbols with a call to \code{addXY}. Note
#' that this is only necessary if the color of the symbol and the color of the
#' error bars are different.\cr
#' The symbol drawn for errors bars in the explantion does not have caps due to a
#' limitation in the system for creating the explanation.
#' 
#' @seealso \code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{addXY}},
#' \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- seq(1, 9, by=1.0)
#' Y1 <- runif(9)
#' Y2 <- runif(9)
#' Y <- (Y1 + Y2)/2
#' Ymin <- pmin(Y1, Y2)
#' Ymax <- pmax(Y1, Y2)
#' setGD()
#' AA.pl <- xyPlot(X, Y, yaxis.range=c(0,1))
#' addErrorBars(X,Ymax, Ymin)
#' # For more details of addErrorBars see 
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export addErrorBars
addErrorBars <- function(x, yup, ylo,
                    Bars=list(name="", cap=0.09, width="standard", color="black"),
                    current=list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE)) {
	# Coding history:
	#    2013Sep19 DLLorenz Original Coding
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  x <- numericData(x)
  x <- transData(x, current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  yup <- numericData(yup)
  yup <- transData(yup, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  ylo <- numericData(ylo)
  ylo <- transData(ylo, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  Bars <- setDefaults(Bars, name="", cap=0.09, color="black")
  if(Bars$cap > 0) {
    doCaps <- TRUE
    ## Compute the inches per user unit
    pin <- par('pin')
    usr <- par('usr')
    uin <- pin/c(usr[2L] - usr[1L], usr[4L] - usr[3L])
    xoff <- Bars$cap/2/uin[1L]
  } else
    doCaps <- FALSE
  ## Need to create a function that will set up the explanation correctly
  Plot <- setPlot(list(), name=Bars$name, what="points", type="solid",
                  width=Bars$width, symbol="+", filled=FALSE,
                  size=0.09, color=Bars$color, area.color=Bars$fill,
                  area.border=Bars$outline) # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explan
  plotPars <- explan$current
  for(i in seq(along=(x))) {
    segments(x[i], ylo[i], x[i], yup[i], lwd=plotPars$lwd, col=plotPars$col)
    if(doCaps) {
      segments(x[i]-xoff, ylo[i], x[i]+xoff, ylo[i], lwd=plotPars$lwd, col=plotPars$col)
      segments(x[i]-xoff, yup[i], x[i]+xoff, yup[i], lwd=plotPars$lwd, col=plotPars$col)
    }
  }
  ## Fix the symbol for the explanation
  N <- length(explan$lines$pch)
  explan$lines$pch[N] <- 73 # I
  current$explanation <- explan
  invisible(current)
}
