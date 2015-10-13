#' Biplot
#' 
#' Produce a biplot, which is a plot of two different types of data on the same
#' graph.
#' 
#' The scaling between observations and variables is controlled by
#' \code{Scale}, which can take any value between 0 and 1 or a character string
#' indicating a specific scaling. The options for the character string are:
#' "distance," which produces a plot where the observations retain their
#' approximate relation with respect to Eucldiean distances and correponds to a
#' numeric value of 1; "variance," which produces a plot where the cosine of
#' the angle between the variable vectors is related to the correlation between
#' the variables and corresponds to a numeric value of 0; "symmetric," which
#' tries to balance the range of values for observations and variables to give
#' a pleasing graph and corresponds to a numeric value of 0.5; or "Auto," which
#' is the same as "variance." Another option for \code{Scale} is
#' "interpolative," which produces a specialized axis scaling so that the
#' approximate values of the variables can be obtained for each observation. It
#' is not implemented in this version.\cr
#' 
#' The \code{obsPlotLabels} and \code{varPlotLabels} arguments must be tagged
#' lists with these components: \describe{ \item{labels}{the labels. For
#' \code{xPlotLabels}, "rownames" means use the row names from \code{x} to
#' generate the labels. For \code{yPlotLabels}, "colnames" means use the column
#' names from \code{y} to generate the labels. Otherwise a character vector of
#' the labels.} \item{dir}{the direction the label text is placed from the
#' object.} \item{size}{the size of the label text.} \item{offset}{the distance
#' the labels is placed relative to the object.} \item{color}{the color of the
#' label text.} }
#' 
#' @param x an object of class "princomp" that has the information to create
#' a biplot.
#' @param Which sequence of two numbers indicating which components to plot.
#' @param Scale either a character string indicating the scaling option between
#' observations and variables, or numeric value controling the scaling. If
#' character, then must be one of "auto," "distance," "symmetric," "variance,"
#' or "interpolative." See \bold{Details}.
#' @param obsPlot control information to plot the observations.  See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param varPlot control information to plot the variables.  See
#' \code{\link{setPlot}} for a description of the parameters. For \code{yPlot},
#' symbol can be "arrow" to indicate that an arrow is to be drawn from the
#' origin.
#' @param obsPlotLabels control information for the observation labels. See
#' \bold{Details}.
#' @param varPlotLabels control information for the variable labels. See
#' \bold{Details}.
#' @param ylabels set y-axis labels for the observation data.
#' @param xlabels set x-axis labels for the observation data.
#' @param ylabels2 set y-axis labels for the variable data.
#' @param xlabels2 set x-axis labels for the variable data.
#' @param xtitle x-axis title (also called x-axis caption) for the observation data.
#' @param ytitle y-axis title (also called y-axis caption) for the observation data.
#' @param xtitle2 x-axis title (also called x-axis caption) for the variable data.
#' @param ytitle2 y-axis title (also called y-axis caption) for the variable data.
#' @param range.factor a numeric factor by which to expand the axis ranges so
#'that labels can be drawn.
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @param \dots not used, required for other methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{biPlot}.
#' @seealso \code{\link{setPage}}, \code{\link{biPlot}}
#' @references Gower, J.C. and Hand, D.J., 1996, Biplots, Chapman and Hall, London, 277 p.
#' @keywords hplot
#' @examples
#' \dontrun{
#' # See for examples of biPlot:
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' }
#' @export
#' @method biPlot princomp
biPlot.princomp <- function(x, Which=1:2, # data
                            Scale='Auto',
                            obsPlot=list(name="observations", what='points', 
                              type='solid', width='standard', symbol='circle', 
                              filled=TRUE, size=0.05, color='black'),
                            varPlot=list(name='variables', width='color',
                              size=0.2, color='darkblue', symbol='arrow',
                              filled=FALSE), # plot controls
                            obsPlotLabels=list(labels='rownames', dir='NE',
                              size=8, offset=0.75),
                            varPlotLabels=list(labels='colnames', dir='Auto',
                              size=8, offset=0.75, color='darkblue'), # object labels
                            ylabels=5,  xlabels=5,
                            ylabels2='Auto',  xlabels2='Auto', # axis labels
                            xtitle='Auto', ytitle='Auto', 
                            xtitle2='Auto', ytitle2='Auto', # axis titles
														range.factor=1.25,
                            caption="", # caption
                            margin=c(NA, NA, NA, NA), ...) {
	# Coding History:
	#    2010Mar18 DLLorenz Original coding, from the revised biplot.princomp code
	#    2011Apr15 DLLorenz Begin modifications for R
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  if(length(Which) != 2L)
    stop("length of choices must be 2")
  scores <- x$scores
  if(!length(scores))
    stop("x must contain scores")
  loadings <- x$loadings
  if(!length(loadings))
    stop("x must contain loadings")
  ## may add options as new classes are created
  lambdas <- x$sdev
  ## prepare the data
  obs <- scores[, Which]
  vars <- loadings[, Which]
  lambdas <- lambdas[Which]
  p <- dim(vars)[1L]
  n <- dim(obs)[1L]
  if(is.character(Scale)) {
    Scale <- match.arg(tolower(Scale), c("auto", "distance", "symmetric",
                                         "variance", "interpolative"))
    if(Scale == "auto")
      Scale <- "variance"
    ## set titles if necessary
    if(all(x$scale == 1))
      scaled <- "" # raw data
    else
      scaled <- "scaled " # these have been scaled to mean 0 and sd 1.
    if(xtitle == 'Auto')
      xtitle <- switch(Scale ,
                       distance=paste("Component ", Which[1L], " (", scaled,
                         "Euclidean distance)", sep=''),
                       variance=paste("Component ", Which[1L], " (", scaled,
                         "Mahalanobis distance)", sep=''),
                       symmetric=paste("Component ", Which[1L], sep=''),
                       interpolative=paste("Component ", Which[1L], " (", scaled,
                         "Euclidean distance)", sep=''))
    if(ytitle == 'Auto')
      ytitle <- switch(Scale ,
                       distance=paste("Component ", Which[2L], " (", scaled,
                         "Euclidean distance)", sep=''),
                       variance=paste("Component ", Which[2L], " (", scaled,
                         "Mahalanobis distance)", sep=''),
                       symmetric=paste("Component ", Which[2L], sep=''),
                       interpolative=paste("Component ", Which[2L], " (", scaled,
                         "Euclidean distance)", sep=''))
    if(xtitle2 == 'Auto')
      xtitle2 <- switch(Scale ,
                        distance=paste(scaled, "variable contribution", sep=''),
                        variance=paste(scaled, "variable standard deviation", sep=''),
                        symmetric="",
                        interpolative="")
    if(ytitle2 == 'Auto')
      ytitle2 <- switch(Scale ,
                        distance=paste(scaled, "variable contribution", sep=''),
                        variance=paste(scaled, "variable standard deviation", sep=''),
                        symmetric="",
                        interpolative="")
    ## set margins 3 and 4 (to default value that allow reasonable room)
    margin[4] <- margin[3] <- switch(Scale,
                                     distance=-4.1, variance=-4.1,
                                     symmetric=0.5, interpolative=0.5)
    ## Set scale to numeric value for correct computation of obs and var
    scale <- switch(Scale,
                    distance=1, variance=0,
                    symmetric=0.5, interpolative=-1)
  }
  else {
    scale <- Scale # better be numeric
    ## Now set the titles and top and right margins
    if(xtitle == 'Auto')
      xtitle <- paste("Component ", Which[1L], sep='')
    if(ytitle == 'Auto')
      ytitle <- paste("Component ", Which[2L], sep='')
    if(xtitle2 == 'Auto')
      xtitle2 <- ""
    if(ytitle2 == 'Auto')
      ytitle2 <- ""
    margin[4] <- margin[3L] <- 0.5
  }
  if(scale < 0) { # this produces an interpolation biplot
    vars <- vars # this needs work, see Joliffe
    warning("Interpolation not yet implemented")
  }
  else if(scale >= 0 && scale <= 1) {
    vars <- vars * rep(lambdas^(1 - scale), c(p, p))
    obs <- obs * rep(lambdas^(scale - 1), c(n, n))
  }
  else stop("bad value for scale")
  ## Call biplot.default to do the plotting!
  invisible(biPlot.default(obs, vars, is.character(Scale),
                           xPlot=obsPlot, yPlot=varPlot,
                           xPlotLabels=obsPlotLabels,
                           yPlotLabels=varPlotLabels,
                           ylabels=ylabels, xlabels=xlabels,
                           ylabels2=ylabels2, xlabels2=xlabels2,
                           xtitle=xtitle, ytitle=ytitle,
                           xtitle2=xtitle2, ytitle2=ytitle2,
  												 range.factor=range.factor,
                           caption=caption, margin=margin))
}

#' @rdname biPlot.princomp
#' @export
#' @method biPlot prcomp
biPlot.prcomp <- function(x, Which=1:2, # data
														Scale='Auto',
														obsPlot=list(name="observations", what='points', 
																				 type='solid', width='standard', symbol='circle', 
																				 filled=TRUE, size=0.05, color='black'),
														varPlot=list(name='variables', width='color',
																				 size=0.2, color='darkblue', symbol='arrow',
																				 filled=FALSE), # plot controls
														obsPlotLabels=list(labels='rownames', dir='NE',
																							 size=8, offset=0.75),
														varPlotLabels=list(labels='colnames', dir='Auto',
																							 size=8, offset=0.75, color='darkblue'), # object labels
														ylabels=5,  xlabels=5,
														ylabels2='Auto',  xlabels2='Auto', # axis labels
														xtitle='Auto', ytitle='Auto', 
														xtitle2='Auto', ytitle2='Auto', # axis titles
														range.factor=1.25,
														caption="", # caption
														margin=c(NA, NA, NA, NA), ...) {
	#
	if(length(Which) != 2L)
		stop("length of choices must be 2")
	scores <- x$x
	if(!length(scores))
		stop("x must contain scores")
	loadings <- x$rotation
	if(!length(loadings))
		stop("x must contain loadings")
	## may add options as new classes are created
	lambdas <- x$sdev
	## prepare the data
	obs <- scores[, Which]
	vars <- loadings[, Which]
	lambdas <- lambdas[Which]
	p <- dim(vars)[1L]
	n <- dim(obs)[1L]
	if(is.character(Scale)) {
		Scale <- match.arg(tolower(Scale), c("auto", "distance", "symmetric",
																				 "variance", "interpolative"))
		if(Scale == "auto")
			Scale <- "variance"
		## set titles if necessary
		if(all(x$scale == 1))
			scaled <- "" # raw data
		else
			scaled <- "scaled " # these have been scaled to mean 0 and sd 1.
		if(xtitle == 'Auto')
			xtitle <- switch(Scale ,
											 distance=paste("Component ", Which[1L], " (", scaled,
											 							 "Euclidean distance)", sep=''),
											 variance=paste("Component ", Which[1L], " (", scaled,
											 							 "Mahalanobis distance)", sep=''),
											 symmetric=paste("Component ", Which[1L], sep=''),
											 interpolative=paste("Component ", Which[1L], " (", scaled,
											 										"Euclidean distance)", sep=''))
		if(ytitle == 'Auto')
			ytitle <- switch(Scale ,
											 distance=paste("Component ", Which[2L], " (", scaled,
											 							 "Euclidean distance)", sep=''),
											 variance=paste("Component ", Which[2L], " (", scaled,
											 							 "Mahalanobis distance)", sep=''),
											 symmetric=paste("Component ", Which[2L], sep=''),
											 interpolative=paste("Component ", Which[2L], " (", scaled,
											 										"Euclidean distance)", sep=''))
		if(xtitle2 == 'Auto')
			xtitle2 <- switch(Scale ,
												distance=paste(scaled, "variable contribution", sep=''),
												variance=paste(scaled, "variable standard deviation", sep=''),
												symmetric="",
												interpolative="")
		if(ytitle2 == 'Auto')
			ytitle2 <- switch(Scale ,
												distance=paste(scaled, "variable contribution", sep=''),
												variance=paste(scaled, "variable standard deviation", sep=''),
												symmetric="",
												interpolative="")
		## set margins 3 and 4 (to default value that allow reasonable room)
		margin[4] <- margin[3] <- switch(Scale,
																		 distance=-4.1, variance=-4.1,
																		 symmetric=0.5, interpolative=0.5)
		## Set scale to numeric value for correct computation of obs and var
		scale <- switch(Scale,
										distance=1, variance=0,
										symmetric=0.5, interpolative=-1)
	}
	else {
		scale <- Scale # better be numeric
		## Now set the titles and top and right margins
		if(xtitle == 'Auto')
			xtitle <- paste("Component ", Which[1L], sep='')
		if(ytitle == 'Auto')
			ytitle <- paste("Component ", Which[2L], sep='')
		if(xtitle2 == 'Auto')
			xtitle2 <- ""
		if(ytitle2 == 'Auto')
			ytitle2 <- ""
		margin[4] <- margin[3L] <- 0.5
	}
	if(scale < 0) { # this produces an interpolation biplot
		vars <- vars # this needs work, see Joliffe
		warning("Interpolation not yet implemented")
	}
	else if(scale >= 0 && scale <= 1) {
		vars <- vars * rep(lambdas^(1 - scale), c(p, p))
		obs <- obs * rep(lambdas^(scale - 1), c(n, n))
	}
	else stop("bad value for scale")
	## Call biplot.default to do the plotting!
	invisible(biPlot.default(obs, vars, is.character(Scale),
													 xPlot=obsPlot, yPlot=varPlot,
													 xPlotLabels=obsPlotLabels,
													 yPlotLabels=varPlotLabels,
													 ylabels=ylabels, xlabels=xlabels,
													 ylabels2=ylabels2, xlabels2=xlabels2,
													 xtitle=xtitle, ytitle=ytitle,
													 xtitle2=xtitle2, ytitle2=ytitle2,
													 range.factor=range.factor,
													 caption=caption, margin=margin))
}
