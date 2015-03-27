#' Axis Ticks and Labels
#' 
#' Add axis ticks, labels, and title
#' 
#' The \code{current} argument is generally the output from a high-level
#' plotting function in \code{smwrGraphs}. If \code{which} is "left" or "right,"
#' then \code{current} must contain a component named \code{yax}. If  \code{which} is
#' "bottom" or "top," then \code{current} must containa component named \code{xax}.
#' Those components are generally constructed from functions like \code{linearPretty} or
#' or \code{logPretty}.
#' 
#' @param which which axis to label, must be one of "bottom," "left," "top," or
#' "right."
#' @param current a list containing the current plot information, see \bold{Details}.
#' @param title the axis title.
#' @param ticks draw the ticks.
#' @param labels draw the labels.
#' @return The current plot information is returned invisibly.
#' @note In general, all functions that create plots will draw the necessary
#' axes. This function should be used only to add axis labels to an unlabeled
#' axis. Axis labels can be suppressed by setting up the margins with negative
#' values.
#' @seealso \code{\link{linearPretty}}, \code{\link{logPretty}},
#' \code{\link{datePretty}}, \code{\link{transPretty}}, \code{\link{addLabel}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' # See for examples of addAxisLabels:
#' vignette(topic="GraphSetup", package="smwrGraphs")
#' }
#' @export addAxisLabels
addAxisLabels <- function(which, current, title="", ticks=FALSE, labels=TRUE) {
	# Coding History:
	#    2011Aug03 DLLorenz Original coding.
	#    2014Jun26 DLLorenz Converted to roxygen
  ##
  which <- match.arg(which, c("bottom", "left", "top", "right"))
  thisside=list(ticks=ticks, labels=labels, grid=FALSE, finegrid=FALSE, angle=0)
  othside=list(ticks=FALSE, labels=FALSE, grid=FALSE, finegrid=FALSE, angle=0)
  switch(which,
         bottom=renderX(current$xax, bottom=thisside, top=othside, bottitle=title),
         top=renderX(current$xax, bottom=othside, top=thisside, bottitle='',
           toptitle=title),
         left=renderY(current$yax, left=thisside, right=othside, lefttitle=title),
         right=renderY(current$yax, left=othside, right=thisside, lefttitle='',
           righttitle=title))
  invisible(current)
}
