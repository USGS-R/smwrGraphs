#' Tree Graphs
#' 
#' Produce a tree graph from a hiearchical cluster analysis.
#' 
#' @details \code{Tree} is a list with these components: 
#' \describe{ 
#' \item{orientation}{the orientation of the dendrogram, must be either "vertical" or
#' "horizontal"}
#' \item{width}{the line width of the line representing the dendrogram}
#' \item{color}{the color of the line representing the dendrogram}}
#' 
#' @param x the data to plot. Must be able to be converted to class 
#'"dendrogram."
#' @param Tree control parameters of the tree diagram. See \bold{Details}.
#' @param axis.range set the range of the tree-axis.
#' @param labels set the tree-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xtitle the x-axis title (also called x-axis caption).
#' @param ytitle the y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{ecdfPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{probPlot}}
#' @keywords hplot
#' @examples
#' \dontrun{
#' # For an example of a dendGram  see
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export dendGram
dendGram <- function(x, # data specification
										 Tree=list(orientation="vertical",
										 					width="standard",
										 					color="black"), # plot controls
										 axis.range=c(NA, NA), # tree-axis range
										 labels="Auto",  # tree-axis labels
										 ytitle="",
										 xtitle="", # axis titles
										 caption="", # caption
										 margin=c(NA, NA, NA, NA)) { # margin control
	## Convert x to dendrogram and get the ranges
	x <- as.dendrogram(x)
	xlab <- labels(x) # the individuals, in order
	xmax <- attr(x, "height")
	# set the defaults for Tree
	Tree <- setDefaults(Tree, orientation="vertical", width="standard",
											color="black")
	lwd <- lineWt(Tree$width)
	col <- Tree$color
	if(dev.cur() == 1L)
		setGD("Tree")
	if(Tree$orientation == "vertical") {
		if(any(is.na(axis.range))) {
			yax <- linearPretty(c(0, xmax), labels=labels, extend.range=FALSE)
		} else {
			yax <- linearPretty(axis.range, hard=TRUE, labels=labels, 
													extend.range=FALSE)
		}
		xax <- namePretty(xlab, orientation="grid", offset=1)
		if(is.na(margin[1L])) # Bottom margin
			margin[1L] <- xax$margin
		if(is.na(margin[1L])) # Top margin (supress)
			margin[1L] <- -1.5
		if(is.na(margin[1L])) # Right margin (supress)
			margin[1L] <- -.5
		margin.control <- setMargin(margin, yax)
		margin <- margin.control$margin
		right <- margin.control$right
		top <- margin.control$top
		left <- margin.control$left
		bot <- margin.control$bot
		bot$angle <- 90 # Use this logic
		# draw it
		plot(x, leaflab="none", edgePar=list(lwd=lwd, col=col), 
				 xlim=xax$range,  yaxt="n", yaxs="i", xaxs="i",
				 ylim=yax$range)
		## label the axes
		renderY(yax, lefttitle=ytitle, left=list(ticks = TRUE, labels = TRUE,
																						 grid = FALSE, finegrid = FALSE,
																						 extend=TRUE),
						right=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
											 finegrid = FALSE))
		renderX(xax, bottitle=xtitle, bottom=list(ticks = FALSE, labels = TRUE,
																							grid = FALSE, finegrid = FALSE,
																							extend = FALSE, angle=90),
						top=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
										 finegrid = FALSE), caption=caption)
	} else { # orientation is horizontal
		if(any(is.na(axis.range))) {
			xax <- linearPretty(c(0, xmax), labels=labels, extend.range=FALSE)
		} else {
			xax <- linearPretty(axis.range, hard=TRUE, labels=labels, 
													extend.range=FALSE)
		}
		yax <- namePretty(xlab, orientation="grid", offset=1)
		if(is.na(margin[1L])) # Top margin (supress)
			margin[1L] <- -1.5
		if(is.na(margin[1L])) # Right margin (supress)
			margin[1L] <- -.5
		margin.control <- setMargin(margin, yax)
		margin <- margin.control$margin
		right <- margin.control$right
		top <- margin.control$top
		left <- margin.control$left
		bot <- margin.control$bot
		# draw it
		plot(x, leaflab="none", edgePar=list(lwd=lwd,col=col), horiz=TRUE,
				 xlim=xax$range,  yaxt="n", yaxs="i", xaxs="i",
				 ylim=yax$range)
		## label the axes
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
		
	}
	invisible(list(x=x, yaxis.log=FALSE, yaxis.rev=FALSE,
                 xaxis.log=FALSE, explanation=list(), margin=margin,
                 yax=yax, xax=xax))
}
