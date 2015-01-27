#' Graph Layout
#' 
#' Set up the layout of one or more graphs on a page.
#' 
#' @details The layout of multple graphs on a page is always set up as a rectangular
#' grid. The columns can be specified in one of two ways, either by specifying
#' the width of each column using \code{width} or setting equal-width columns
#' by specifying \code{num.cols}. The rows can be specified in one of two ways,
#' either by specifying the height of each column using \code{height} or
#' setting equal-height rows by specifying \code{num.rows}.\cr 
#' 
#' If an explanation is to be placed outside of the graphs, then \code{explanation}
#' is used to indicate where the explanation is to be placed. The explanation
#' can be placed either to the right of the grid of graphs, at the bottom of
#' the grid, or in one of the grid cells.\cr 
#' 
#' To place an explanation to the
#' right of the graphs, \code{explantion} should be set to
#' \code{list(right=ewid)}, where \code{ewid} is the width of the explanation.
#' In this case, the total of \code{width} and \code{ewid} must be less that
#' the total available for the page.\cr 
#' 
#' To place an explanation at the bottom
#' of the graphs, \code{explantion} should be set to \code{list(bottom=ehei)},
#' where \code{ehei} is the height of the explanation. In this case, the total
#' pf \code{height} and \code{ehei} must be less than the total available for
#' the page.\cr 
#' 
#' To place an explanation within a cell of the grid,
#' \code{explantion} should be set to \code{list(grid=enum)}, where \code{enum}
#' is the cell number in the grid. Cell numbers are sequential starting in the
#' upper left and increasing by column. In this case \code{num.graphs} must be
#' set to some number less than \code{num.cols} times \code{num.rows}.\cr 
#' 
#' The width of the explanation can be estimated by allocating 1 inch per 13
#' characters for \code{font} set to "preview" or 1 inch per 17 characters for
#' \code{font} set to "USGS" plus 0.5 inch for the symbols. The width for box
#' plots should be 2 inches for any type other that "tukey" and 2.5 inches for
#' "tukey." The height of the explanation can be estimated as 1 inch per 8
#' lines of explanation for \code{font} set to "preview" and 1 inch per 10
#' lines of explanation for \code{font} set to "USGS"---allocate an extra 2
#' lines for the title. Box plots require about 3 inches for the truncated and
#' simple types and about 4.5 inches for type "tukey" and about 4 inches for
#' type "extended." The plot area within each cell is set up to have consistent
#' widths within each column and consistent heights within each row. The
#' arguments \code{shared.x}, \code{shared.y}, \code{yleft}, \code{yright},
#' \code{xbottom}, and \code{xtop} are used to set up the plot areas. If axes
#' are not shared, then the margin values are not set for any graph. If the
#' axes are shared, then the margin values apply to the corresponding left
#' column, right column, bottom row or top row.\cr 
#' 
#' The axis ticks and labels can be supressed by setting the margins to a negative 
#' value.  This is most useful when adding right-axes with \code{addXY} for example.\cr
#' 
#' The value for \code{yright} can be set using the \code{setRtMargin} 
#' function if adding a plot using the secondary right axes; extract 
#' the fourth element of the returned value. The default is to 
#' set a narrow right-hand margin.
#' 
#' The value for \code{xtop} can be set to -2.2 if adding a plot using the 
#' secondary top axis.
#' The default is to set the margin to 1.5, which allows for a graph title.
#' 
#' @param width the width of the graph area, exclusive of any explanation. Can
#' be either the total width or the width of each column of graphs. If NULL,
#' then use default figure width. See \bold{Details}.
#' @param height the height of the graph area, exclusive of explanation. Can be
#' either the total width or the height of each row of graphs. If NULL, then
#' use defualt figure height.
#' @param num.cols the number of columns in the rectangular array of graphs.
#' Computed from width if not supplied.
#' @param num.rows the number of rows in the rectangular array of graphs.
#' Computed from height if not supplied.
#' @param num.graphs the number of actual graphs in the rectangular array of
#' graphs.
#' 
#' Computed from \code{num.cols} and \code{num.rows} if not supplied. Note that
#' if the EXPLANATION is to be placed in one of the array of graphs, then
#' \code{num.graphs} must be less than the product of num.cols and num.rows.
#' @param explanation a description of where to place the explanation if put
#' into a separate graph. See \bold{Details}.
#' @param shared.x indicate how the x axes are to be shared. See
#' \bold{Details}.
#' @param shared.y indicate how the y axes are to be shared. If < 0, then no
#' sharing--each has own axis labels, etc. If = 0, then axes in direct contact.
#' If > 0, then the value indicates the relative spacing.
#' @param yleft space to allocate on the left margin of each graph.
#' @param yright space to allocate on the right margin of each graph. See \bold{Details}.
#' @param xbottom space to allocate on the bottom margin of each graph.
#' @param xtop space to allocate on the top margin of each graph. See \bold{Details}.
#' @return an object of class "Layout" with three named components and 
#' \code{num.graphs} numbered
#' components: \item{explanation and each numbered component}{a list with two
#' components:} \item{margin}{the margin for the plot area} 
#' \item{fig}{the figure area} \item{size}{the size of the overall figure}
#' \item{mat}{the figure layout}
#' @note It is very easy to confuse the graph number, used by \code{setGraph}
#' and the grid cell number referenced in \code{setlayout}. The grid cell
#' number always ranges from 1 to the number of columns times the number of
#' rows. The graph number ranges from 1 to \code{num.graphs} and skips the
#' grid cell number if defined in the \code{explanation} argument. Printing the
#' output object can help understand the graph layout.\cr
#' There is nothing special about the cell allocated for the explanation; it has
#' no special characteristics, therefore an explantion can be placed in any graph
#' numbered cell and anything can be placed in the "explanation" cell. As an example,
#' the "explanation" at the bottom of the figure can be used for a description of 
#' the figure that is more than one line in height. 
#' @seealso \code{\link{setPage}}, \code{\link{setGraph}}, \code{\link{setRtMargin}},
#' \code{\link{addTitle}}
#' @keywords dplot
#' @export setLayout
setLayout <- function(width=NULL, height=NULL, # Size of graphs or graph area
											num.cols=max(1, length(width)),
											num.rows=max(1, length(height)),
											num.graphs=num.rows*num.cols, # Graph grid
											explanation=NULL, # Where to put explanation
											shared.x=-1, shared.y=-1, # Share axes for multiple graphs
											yleft=3.5, yright=NA, # Margins for left and right axes
											xbottom=3.2, xtop=NA) { # Margin for bottom and top axis
	# Coding History:
	#    2008Jun27 DLLorenz Original coding and begin tweaks
	#    2010Nov30 DLLorenz Modified for R - use the layout() function
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Apr27 DLLorenz More grid debugging
	#    2012Aug31 DLLorenz if top/right margins are set, use instead of NAs
	#    2012Dec20 DLLorenz Bug fix for page eject
	#    2013mar29 DLLorenz Tweak for explanation
	#    2014Jun26 DLLorenz Converted to roxygen.
	#    2015Jan02 DLLorenz Bugfix for shared axes and other tweaks.
	##
	mar1 <- as.double(c(NA, NA, xtop, yright))
	## Set up the page
	if(is.null(height)) {
		height <- par('fin')[2]
		if(!is.null(explanation$bottom))
			height <- height - explanation$bottom
	}
	if(is.null(width)) {
		width <- par('fin')[1]
		if(!is.null(explanation$right))
			width <- width - explanation$right
	}
	## Instantiate the potentially computed arguments
	num.cols <- num.cols
	num.rows <- num.rows
	num.graphs <- num.graphs
	## Easy--num.graphs=1
	retval <- list()
	if(num.graphs == 1) {
		if(is.null(explanation)) {
			mat <- matrix(1, nrow=1)
			widths <- width
			heights <- height
		}
		else if(!is.null(explanation$right)) {
			mat <- matrix(c(1, 2), nrow=1)
			widths <- c(width, explanation$right)
			heights <- height
			mar2 <- c(1.2,.5,0,0) # Leave room for caption and right
		}
		else if(!is.null(explanation$bottom)){ # Should be bottom
			mat <- matrix(c(1, 2), nrow=2)
			widths <- width
			heights <- c(height, explanation$bottom)
			mar2 <- c(1.2,0,.5,0)  # Leave room for caption and top
		}
		layout(mat, widths=lcm(widths*2.54), heights=lcm(heights*2.54))
		par(mar=c(0,0,0,0))
		plot.new()
		retval[[1]] <- list(margin=mar1, fig=par('fig'))
		if(!is.null(explanation)) { # put the explanation info in
			plot.new()
			retval$explanation <- list(margin=mar2, fig=par('fig'))
		}
		retval$size <- c(sum(widths), sum(heights))
		par(new=TRUE)
		return(retval)
	} # Done with a single graph and optional explanation
	## Note that 1 margin line is par('csi')--set up lengths
	## The 0.6 adjustment is not 100% clear, but the cex value changes after the
	## first call to a plot (to 0.66). Go figure.
	## If there ever comes a time when the boxes in a grid layout look funny,
	## look here first!
	lineSize <- par('csi') * 0.8
	margins <- array(rep(mar1, each=num.rows*num.cols),
									 dim=c(num.rows, num.cols, 4L))
	## Set margins:
	## For columns set the internal margins based on shared.y
	if(shared.y >= 0) {
		if(is.na(yright)) # Set to 0.5
			yright <- 0.5
		margins[, 1L, 2L] <- yleft
		margins[, 2:num.cols, 2L] <- shared.y / 2
		margins[, num.cols, 4L] <- yright
		margins[, 1:(num.cols - 1), 4L] <- shared.y / 2
		if(shared.x < 0) { # Need to populate margins so that graphs align
			if(is.na(xtop))
				xtop <- 1.5 # Allow room for graph title
			margins[1:num.rows, , 3L] <- xtop
			margins[1:num.rows, , 1L] <- xbottom
		}
	} # Otherwise leave NA
	## For rows set the internal margins based on shared.x
	if(shared.x >= 0) {
		if(is.na(xtop)) # Set to 0.5, no title graph allowed
			xtop <- 0.5
		margins[1L, , 3L] <- xtop
		margins[2:num.rows, , 3L] <- shared.x / 2
		margins[num.rows, , 1L] <- xbottom
		margins[1:(num.rows - 1), , 1L] <- shared.x / 2
		if(shared.y < 0) { # Need to populate margins so the graphs align
			if(is.na(yright)) # Set to 0.5
				yright <- 0.5
			margins[, 1:num.cols, 2L] <- yleft
			margins[, 1:num.cols, 4L] <- yright
		}
	} # otherwise leave NA
	## Automagically allocate width and height of grid cells, if requested.
	## Height of rows first
	if(length(height) == 1L && num.rows > 1L) {
		if(shared.x < 0) {
			if(is.na(xtop))
				xtop <- 1.5
			topspace <- rep(xtop, num.rows)
			botspace <- rep(xbottom, num.rows)
			if(is.null(explanation$bottom)) # Need to provide for caption
				botspace[num.rows] <- xbottom + 1
		}
		else { # Works for all non negative values of shared.x
			topspace <- c(xtop, rep(shared.x / 2, num.rows - 1))
			botspace <- c(rep(shared.x / 2, num.rows - 1), xbottom)
			if(is.null(explanation$bottom)) # Need to provide for caption
				botspace[num.rows] <- xbottom + 1
		}
		dist.h <- (height - sum(topspace + botspace) * lineSize) / num.rows
		height <- dist.h + (topspace + botspace) * lineSize
	}
	## Width of columns second
	if(length(width) == 1 && num.cols > 1) {
		if(shared.y < 0) {
			if(is.na(yright))
				yright <- 0.5
			leftspace <- rep(yleft, num.cols)
			rghtspace <- rep(yright, num.cols)
		}
		else { # Works for all non negative values of shared.y
			leftspace <- c(yleft, rep(shared.y / 2, num.cols - 1))
			rghtspace <- c(rep(shared.y / 2, num.cols - 1), yright)
		}
		dist.w <- (width - sum(leftspace + rghtspace) * lineSize) / num.cols
		width <- dist.w + (leftspace + rghtspace) * lineSize
	}
	## Set up layout initial matrix (exclude right or bottom explanation)
	Seq <- seq(1, num.cols*num.rows)
	if(!is.null(explanation) && !is.null(explanation$grid)) {
		Egrid <- explanation$grid
		## Check to see that num.graphs < num.rows * num.cols
		if(num.graphs >= num.rows * num.cols)
			stop('No room for explanation in grid')
		Seq[Seq > Egrid] <- Seq[Seq > Egrid] - 1
		Seq[Egrid] <- num.graphs + 1
		Seq[Seq > (num.graphs + 1)] <- 0
	}
	else # Explanation somewhere else
		Seq[Seq > num.graphs] <- 0
	mat <- matrix(Seq, num.rows, byrow=TRUE)
	if(is.null(explanation) || !is.null(explanation$grid)) { # No augmentation
		widths <- width
		heights <- height
		ckh <- heights
	} else if(!is.null(wid <- explanation$right)) { # Put on right
		widths <- c(width, wid)
		heights <- height
		ckh <- heights
		mat <- cbind(mat, num.graphs+1)
	} else if(!is.null(bot <- explanation$bottom)) { # Put on bottom
		widths <- width
		ckh <- height # Allow a thin bottom explanation for specialized purposes
		heights <- c(height, bot)
		mat <- rbind(mat, num.graphs+1)
	} else
		stop('Invalid explanation')
	## Check for error in plot size--too small for text size
	if(min(c(widths, ckh)) < 6.5 * lineSize)
		stop('Too many graphs for page/figure size')
	layout(mat, widths=lcm(widths*2.54), heights=lcm(heights*2.54))
	## Populate the returned index of margins and graphs areas
	graphcount <- 0
	par(mar=c(0,0,0,0))
	for(i in seq(num.rows))
		for(j in seq(num.cols))
			if(mat[i,j] <= num.graphs && mat[i,j] > 0) {
				plot.new()
				retval[[mat[i,j]]] <- list(margin=margins[i,j,], fig=par('fig'))
				graphcount <- graphcount + 1
			}
	## Reset cex so that the margins are actually set correctly!
	## This si the change of Apr27
	par(cex=1)
	## Now figure out what to do with the explanation
	if(is.null(explanation))
		## If null, then copy last grid into the explanation tag
		##  (avoids lazy explanation)
		retval$explanation <- retval[[graphcount]]
	else { # Put the explanation in the correct place
		plot.new()
		## Set the explanation
		retval$explanation <- list(margin=rep(0,4), fig=par('fig'))
	}
	## Finally the size of the total graph area and other info
	retval$size <- c(sum(widths), sum(heights))
	attr(mat, "graphcount") <- graphcount
	retval$mat <- mat
	class(retval) <- "Layout"
	par(new=TRUE)
	return(retval)
}

