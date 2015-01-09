#' Conditioned Graphs
#' 
#' Facilitates producing a series of graphs conditioned by a grouping variable.
#' 
#' Graphs are created by executing \code{plot.call} for each unique value in
#' the variable specified by \code{group}. The columns and rows are set by
#' \code{num.cols} and \code{num.rows}. If both are set to missing \code{NA},
#' then each page contains 1 graph, and the graphics set up should provide for
#' mulitple pages. If one of \code{num.cols} or \code{num.rows} is set to a
#' numeric value, the other is calculated from the number of graphs so that all
#' graphs would be displayed on a single page. If both \code{num.cols} and
#' \code{num.rows} are set to numeric values, then each page would contain a
#' maximum of \code{num.cols} times \code{num.rows} and multiple pages would be
#' needed if that were less than the total number of graphs.\cr
#' 
#' The order of the graphs is controlled by the type of \code{group}. If
#' \code{group} is a factor, then the order is set be the order of the levels,
#' otherwise the order is set by the order from \code{unique}.
#' 
#' @param plot.call either a simple call to a graphics function or a sequence
#' of calls enclosed in curly braces({})
#' @param data the data.frame containing the variables used in \code{plot.call}
#' and \code{group}.
#' @param group a character string identifying the grouping variable in
#' \code{data}.
#' @param format the orientation of the graphs. If "table," then the graphs are
#' created beginning in the upper-left hand corner. If "grid," then the graphs
#' are created beginning in the lower-left See \bold{Details}. hand corner.
#' @param num.cols the number of columns on each page. See \bold{Details}.
#' @param num.rows the number of rows on each page. See \bold{Details}.
#' @param explanation where to place an explanation if necessary. See
#' \code{\link{setLayout}} for details.
#' @param yleft the value for the left margin for the left-hand column. Must be
#' large enough for the labels and title.
#' @param share character, if \code{share} contains "x", then the code in 
#'\code{plot.call} is set up to share x-axes; if \code{share} contains "y", 
#'then the code in \code{plot.call} is set up to share y-axes. The default is
#'not to share either axes.
#' @param group.name a character string to prepend to each value in
#' \code{group} to create the graph title.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @return The value returned by \code{plot.call} is returned invisibly. If
#' used correctly, it could be used to add an explanation.
#' @note This function is desgined to facilitate the production of conditioned
#' graphs, but may not render each collection completely. For example, graphs
#' arranged in table format will not have x-axis labels for incomplete
#' columns.\cr
#' 
#' A call must be made to \code{setPage} or \code{setPDF} to set up the
#' graphics page before calling \code{condition}. The returned value can be
#' used to create an explanation, if desrired. if(\code{explanation} is not
#' \code{NULL}, then the graph is set to the explanation and there is no need
#' to call \code{setGraph} to set up the explanation.\cr
#' 
#' The called plotting function must set the \code{margin} argument to
#' \code{.margin}. See the demos for examples.
#' @seealso \code{\link{setLayout}}, \code{\link{setPage}},
#' \code{\link{setPDF}}, \code{\link{unique}}
#' @references Cleveland, W.S., 1993, Visualizing data: Summit, New Jersey,
#' Hobart Press, 360 p.
#' @keywords hplot
#' @examples
#' 
#' # See the Coplot demos for examples
#' .pager <- options("pager")
#' options(pager="console")
#' demo(package="smwrGraphs")
#' options(.pager)
#' 
#' @export condition
condition <- function(plot.call, data, group, format="grid",
                      num.cols=NA, num.rows=NA , explanation=NULL,
                      yleft=3.5, share="", group.name="",
                      xtitle="", ytitle="", caption="") {
	# Coding history:
	#    2012Nov01 DLLorenz Original scratch idea
	#    2013Mar29 DLLorenz Added to package
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  ## Get the group data
  ## This works if group is expressed as a name
  group <- eval(group, envir=data)
  if(length(group) == 1) # treat as character and extract from data
    group <- data[[group]]
  if(is.factor(group))
    gps <- levels(group[, drop=TRUE])
  else
    gps <- unique(group)
  format <- match.arg(format, c("table", "grid"))
  N <- length(gps)
  if(is.na(num.cols) && is.na(num.rows)) { # set both to 1
    num.cols <- 1L
    num.rows <- 1L
  }
  else if(is.na(num.cols) && !is.na(num.rows)) # make enough for 1 page
    num.cols <- (N + num.rows - 1) %/% num.rows
  else if(!is.na(num.cols) && is.na(num.rows)) # ditto
    num.rows <- (N + num.cols - 1) %/% num.cols
  GrMax <- num.cols * num.rows
  shx <- -1
  if(num.rows > 1L && share %cn% "x") {
    shx <- 1.1
  }	
  shy <- -1
  if(num.cols > 1L && share %cn% "y") {
    shy <- 1.1
  }
  Grkey <- GrMax - num.cols + 1L
  ToDo <- 0L
  Fig <- par("fig")
  for(i in gps) {
    if(ToDo == 0L) { # need to set up layout
      AA.lo <- setLayout(num.rows=num.rows, num.cols=num.cols,
                         shared.x=shx, shared.y=shy, yleft=yleft,
                         explanation=explanation, xtop=1.0)
      ToDo <- 1L
    }
    if(format == "table")
      grd <- ToDo
    else
      grd <- (((GrMax + num.cols) - ToDo) %/% num.cols - 1) * num.cols +
        (ToDo + num.cols - 1) %% num.cols + 1
    AA.gr <- setGraph(grd, AA.lo)
    assign(".margin", AA.gr, pos=1)
    retval <- eval(substitute(plot.call), envir=data[group == i, ],
         enclos=.GlobalEnv)
    ## Add a graph title by brute force
    mtext(paste(group.name, i, sep=""), side=3, line=0.3, adj=0, family="USGS", cex=9/8)
    ## Add the caption if ToDo is 1, and add titles if requested
    if(grd == Grkey) {
      addCaption(caption)
      if(xtitle != "" || ytitle != "") {
        ## Need to make sure that the base line for the figure is OK
        Fig1 <- Fig
        Fig1[3L] <- par("fig")[3L]
        par(fig=Fig1, mar=c(AA.gr[1:2], 1, .5), # captures the upper and right
            usr=c(0,1,0,1))
        if(xtitle != "")
          mtext(text=xtitle, side=1, line=AA.gr[1L] - 2.1, adj=0.5,
                at=0.5, family="USGS", cex=9/8)
        if(ytitle != "")
          mtext(text=ytitle, side=2, line=AA.gr[2L] - 1.75, las=0, adj=0.5,
                at=0.5, family="USGS", cex=9/8)
      }
    }
    ToDo <- ToDo + 1L
    if(ToDo > GrMax)
      ToDo <- 0L
  }
  if(!is.null(explanation))
    setGraph("explanation", AA.lo)
  invisible(retval)
}
