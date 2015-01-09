#' Report Graph
#' 
#' Create a report of any R object in a graph.
#'
#' @details The value for \code{family} cna be any valid font family for the
#'device. In general "serif," "sans," "mono," and "USGS" are valid. The default,
#'"Auto" selects "USGS" for character vectors, and "mono" for any other object. 
#'
#' @param x any R object.
#' @param family the font family to use in the report. See \bold{Details}.
#' @param size the size of the text, in points
#' @return In contrast to other high-level graphics functions in the \code{smwrGraphs}
#'package, this function returns nothing because nothing is expected to be added to
#'the graph and nothing contributed to a possible explanation.
#' @note The report is always placed in the upper left hand corner of the graph and is 
#'left justified. If the report is longer than the height of the graph or wider than
#'the width of the graph, then the report is truncated.
#' @export
reportGraph <- function(x, family="Auto", size = 60 * par("csi")) {
  ## Coding history:
  ##    2015Jan08 DLLorenz Original version
  ##
  if (dev.cur() == 1) 
    setGD("ReportGraph")
  if(class(x)[1L] != "character") {
    txt <- capture.output(x)
    if(family == "Auto")
      family="mono"
  } else {
    txt <- x
    if(family == "Auto")
      family="USGS"
  }
  ## Draw the text
  plot.new()
  par(mar=c(0.5,0.5,0.5,0.5), usr=c(0,1,0,1))
  text(0, 1, paste(txt, collapse="\n"), family="mono", adj=c(0,1),
       cex=size/(60 * par("csi")))
  return(invisible())
}
