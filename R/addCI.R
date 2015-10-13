#' Add Confidence Interval Lines
#' 
#' Computes the confidence interval for a linear regression or q-normal graph and adds the lines to a graph.
#' 
#' 
#' @param type the type of confidence interval desired. Must be either "SLR" for the confidence
#'interval for a simple linear regression model, which must have been created using the
#'\code{addSLR} function or "q-norm" for the confidence interval for a q-normal plot created
#'using \code{qqPlot}.
#' @param level the confidence level desired.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param current the current plot information, normally the output from a high-level
#' graphics function like \code{qqPlot} or \code{xyPlot}.
#' @return The current plot information, the x and y components are the data, not the line.
#'The regression model is included as the lm component.
#' @note The equation for the confidence intervals for a simple linear regression 
#'model can be found in any textbook on regression, see section 9.4.4 in Helsel and 
#'Hirsch (2002) for example.\cr
#' The confidence interval for a normal distribution is described in Appendix 9 in
#'U.S. Water Resources Council (1982). Owen (1968) describes the application of the
#'noncentral \emph{t}-distribution for computing the tolerance limits for a normal
#'distribition.
#' @references
#'Helsel, D.R., and Hirsch, R.M., 2002, Statistical methods in water resources: 
#'U.S. Geological Survey Techniques of Water-Resources Investigations, book 4, 
#'chap. A3, 522 p.\cr
#'U.S. Water Resources Council, 1982, Guidelines for determining flood flow frequency,
#'revised September 1981, Editorial Corrections March 1982: 
#'Hydrology Committee Bulletin 17B, Washington D.C., 190 p., 1 plate.
#' @seealso \code{\link{addSLR}}, \code{\link{qqPlot}}, \code{\link{addErrorBars}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' setGD()
#' AA.pl <- qqPlot(X)
#' addCI("q-norm", current=AA.pl)
#' # For more details of addCI see
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' }
#' @export addCI
addCI <- function(type, level=0.95, # data
									Plot=list(name="", what="lines", type="solid",
														width="standard", color="black"), # plot controls
									current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
															 xaxis.log=FALSE)) { # current plot parameters
	# Coding History:
	#    2014Jul10 DLLorenz Original coding
	#
	# only ones supported in current version
	type <- match.arg(type, c("SLR", "q-norm"))
	newdata <- data.frame(x=seq(current$xax$range[1L], current$xax$range[2L], 
															length.out=101))
	if(type == "SLR") {
		pred <- predict(current$lm, newdata, interval="confidence", level=level)
		pred <- cbind(newdata, pred)
	} else {
		warn <- options("warn")
		options(warn=-1) # suppress inaccurate calculation warning
		alpha <- (1-level)/2
		n <- length(current$y)
		y.mn <- mean(current$y)
		y.sd <- sd(current$y)
		pred <- data.frame(lwr=qt(alpha, n - 1L, sqrt(n)*newdata$x)/sqrt(n) * y.sd + y.mn,
											 upr=qt(1-alpha, n - 1L, sqrt(n)*newdata$x)/sqrt(n) * y.sd + y.mn)
		pred <- cbind(newdata, pred)
		options(warn)
	}
	# OK, plot the lines
	Plot$what <- "lines" # Force lines
	Plot <- setPlot(Plot, name="", what="lines", type="solid",
									width="standard", color="black") # force defaults if not set
	# the data are transformed, so suppress any transformation, but recover on the end
	xlog <- current$xaxis.log
	current$xaxis.log <- FALSE
	yrev <- current$yaxis.rev
	current$yaxis.rev <- FALSE
	ylog <- current$yaxis.log
	current$yaxis.log <- FALSE
	# First, add line without modifying explanation
	addXY(pred$x, pred$lwr, Plot=Plot, current=current)
	# OK, create the explanation
	current <- addXY(pred$x, pred$upr, Plot=Plot, current=current)
	current$xaxis.log <- xlog
	current$yaxis.rev <- yrev
	current$yaxis.log <- ylog
  invisible(current)
}

