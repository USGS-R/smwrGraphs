#' Bar Graph
#' 
#' Create a bar chart by adding bars to an existing graph.
#' 
#' The \code{Bars} argument must be a tagged list with these components:
#' \describe{ \item{name}{a name describing the data; used in the explanation.
#' If "Auto," then derive the name from the column names in \code{y}.}
#' \item{fill}{the name of the color to fill each bar.} \item{outline}{the name
#' of the color to draw the outline or border for each bar. If "none," then no
#' border is drawn.} \item{width}{the width of each bar proportional to the
#' distance between bars. If 1, then the bars form a continuous filled area.
#' The default is "Auto," which fills 2/3 of the distance. If 0, then draw vertical
#' lines rather than bars; the color of the line is based on \code{outline}.}
#' \item{orientation}{the orientation of the bars.} } Only single bars are
#' permitted in the current version. The \code{orientation} component of
#' \code{Bars} is ignored.
#' 
#' @param x the x-coordinate data. Missing values are permitted but result in
#' no bar.
#' @param y the heights of the bars or y-coordinate data. Missing values are
#' permitted but result in no bar.
#' @param base extend the bars from this value.
#' @param Bars parameters defining the characteristics of the bars. See
#' \bold{Details}.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @return The current plot information is returned invisibly.
#' @note Use of \code{addBars} adds 1 step to creating bar charts, but adds
#' flexibility in axis set up from existing high-level plotting functions such
#' as \code{xyPlot} or \code{timePlot}.
#' @seealso \code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{addXY}},
#' \code{\link{xyPlot}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' # See for examples of addBars:
#' demo(topic="AnnualFlowBarChart", package="smwrGraphs")
#' }
#' @export addBars
addBars <- function(x, y, base=0,
                    Bars=list(name="Auto", fill="gray80", outline="black",
                      width="Auto", orientation="stack"),
                    current=list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE)) {
	# Coding history:
	#    2012Nov11 DLLorenz Original Coding
	#    2014Jun26 DLLorenz Converted to roxygen
	#    2015Jan07 DLLorenz Added width = 0 option to Bars
	#
  x <- numericData(x)
  y <- as.matrix(y)
  base <- pmax(transData(base, current$yaxis.log, current$yaxis.rev,
  									current$ytrans, current$ytarg), par("usr")[3L]) # Convert -Inf to min y
  base <- rep(base, length.out=length(x))
  Bars <- setDefaults(Bars, name="Auto", fill="gray80", outline="black",
                      width="Auto", orientation="stack")
  if(ncol(y) == 1L) {
    if(Bars$width == "Auto")
      xoff <- 1/3
    else
      xoff <- Bars$width/2
    ybar <- transData(y[,1], current$yaxis.log, current$yaxis.rev,
    									current$ytrans, current$ytarg)
    if(xoff > 0) {
    	rect(x - xoff, base, x + xoff, ybar, col=Bars$fill,
    			 border=Bars$border, lwd=lineWt("standard"))
    } else
    	segments(x, base, x, ybar, col=Bars$outline, lwd=lineWt("color"))
    if(Bars$name == "Auto") {
    	if(is.null(colnames(y)))
        name <- ""
      else
        name <- colnames(y)
    }
    else
      name <- Bars$name
    if(xoff > 0) {
    	Plot <- setPlot(list(), name=name, what='points', type='solid',
    									width='standard', symbol='none', filled=TRUE,
    									size=0.09, color='black', area.color=Bars$fill,
    									area.border=Bars$outline) # force defaults if not set
    } else
    	Plot <- setPlot(list(), name=name, what='vertical', type='solid',
    									width='standard', symbol='none', filled=TRUE,
    									size=0.09, color=Bars$outline)
    explan <- setExplan(Plot, old=current$explanation) # add info to set up explan
  }
  else
    stop("Current version does not support multiple columns")
  current$x <- x
  current$y <- y
  current$explanation <- explan
  invisible(current)
}

