#' Compute Statistcs for a Box Plot
#' 
#' Compute the statistics for truncated, simple, Tukey, or extended box plots 
#'(support function for \code{boxPlot}).
#' 
#' @param x a list containing the data to compute the statistics.
#' @param Box control paramters for the box.
#' @param yaxis.log logical: log transform data before computing the
#' statistics?
#' @return a list containing the statistics for each box plot.
#' @seealso \code{\link{boxPlot}}
#' @keywords dplot
#' @export boxPlotStats
boxPlotStats <- function(x, Box, yaxis.log) {
	## Arguments:
	##  x (tagged list) the raw data
	##  Box (tagged list) the type of boxplot, etc, must be fully populated before call
	##  yaxis.log (logical scalar) log-tranform the data?
	##
	type <- match.arg(Box$type, c("tukey", "truncated", "simple", "extended"))
	## The text to use for the explanation:
	explain <- list(tukey=list(z=list(stats=matrix(c(-1.2, -0.1,.8, 1.5, 3.6),
																								 ncol=1),
																		n=54, names="",
																		out=-2.6, farout=-3.9,
																		group=c(1, 1)),
														 labels=list(expression(bold("Number of values")),
														 						list(expression(bold("Largest value within 1.5 times")),
														 								 expression(bold("  interquartile range above")),
														 								 expression(bold("  75th percentile"))),
														 						"75th percentile",
														 						"50th percentile\n  (median)",
														 						"25th percentile",
														 						list(expression(bold("Smallest value within 1.5 times")),
														 								 expression(bold("  interquartile range below")),
														 								 expression(bold("  25th percentile"))),
														 						list(expression(paste(bold("Outside value"), symbol("\276"),
														 																	"Value is > 1.5 and", sep='')),
														 								 " < 3  times the interquartile range",
														 								 "  beyond either end of box"),
														 						list(expression(paste(bold("Far-out value"),symbol("\276"),
														 																	"Value ", is >= 3, " times", sep='')),
														 								 "  the interquartile range beyond", "  either end of box")),
														 values=c(4.5, 3.6, 1.5, .7, -0.1, -1.2, -2.6, -3.9),
														 IQR=expression(bold("Interquartile\nrange"))),
									truncated=list(z=list(stats=matrix(c(-3.0, -1.2 ,0, 1.5, 3.5),
																										 ncol=1),
																				n=54, names=""),
																 labels=list(expression(bold("Number of values")),
																 						as.expression(substitute(bold(x), list(x=paste(Box$truncated[2], "th percentile", sep='')))),
																 						expression(bold("75th percentile")),
																 						list(expression(bold("50th percentile")),expression(bold("  (median)"))),
																 						expression(bold("25th percentile")),
																 						as.expression(substitute(bold(x), list(x=paste(Box$truncated[1], "th percentile", sep=''))))),
																 values=c(4.2, 3.5, 1.5, 0, -1.2, -3.0)),
									simple=list(z=list(stats=matrix(c(-3.0, -1.2 ,0, 1.5, 3.5),
																									ncol=1),
																		 n=54, names=""),
															labels=list(expression(bold("Number of values")),
																					expression(bold("Maximum value")),
																					expression(bold("75th percentile")),
																					list(expression(bold("50th percentile")),expression(bold("  (median)"))),
																					expression(bold("25th percentile")),
																					expression(bold("Minimum value"))),
															values=c(4.2, 3.5, 1.5, 0, -1.2, -3.0)),
									extended=list(z=list(stats=matrix(c(-3., -1.5, 0, 1.5, 3.),
																										ncol=1),
																			 n=54, names="",
																			 out=c(-3.5, 4.0), group=c(1,1)),
																labels=list(expression(bold("Number of values")),
																						list(expression(bold("Individual value above the")),
																								 as.expression(substitute(bold(x), list(x=paste("  ",
																								 																							 Box$truncated[2], "th percentile", sep=''))))),
																						as.expression(substitute(bold(x), list(x=paste(Box$truncated[2], "th percentile", sep='')))),
																						expression(bold("75th percentile")),
																						expression(bold("50th percentile\n (median)")),
																						expression(bold("25th percentile")),
																						as.expression(substitute(bold(x), list(x=paste(Box$truncated[1], "th percentile", sep='')))),
																						list(expression(bold("Individual value below the")),
																								 as.expression(substitute(bold(x), list(x=paste("  ",
																								 																							 Box$truncated[1], "th percentile", sep='')))))),
																values=c(4.5, 4.0, 3, 1.5, 0, -1.5, -3., -3.5)))
	bxp.stats <- function(x, range=1.5) {
		stats <- quantile(x, type=2, names=FALSE, na.rm=TRUE)
		if(range > 0) {
			iqr <- diff(stats[c(2, 4)])
			loout <- x < (stats[2] - iqr*range)
			stats[1] <- min(x[!loout])
			hiout <- x > (stats[4] + iqr*range)
			stats[5] <- max(x[!hiout])
			out <- x[loout | hiout]
		}
		else
			out <- numeric(0)
		stats <- as.matrix(stats)
		return(list(stats=stats, out=out, n=sum(!is.na(x))))
	}
	## Create the rest of the explanation
	explan <- setExplan(list(name="boxplot", what="none", type="solid",
													 width="standard", symbol="circle", filled=TRUE,
													 size=0.09, color="black"))
	## Remove NAs from data
	x <- lapply(x, function(xx) xx[!is.na(xx)])
	## Process the data min and max needed for renderBoxPlot
	minx <- min(unlist(x), na.rm=TRUE)
	maxx <- max(unlist(x), na.rm=TRUE)
	## Convert to common log if yaxis.log
	if(yaxis.log) {
		if(minx <= 0)
			stop("All data must be greater than 0 for log scale")
		x <- lapply(x, log10)
	}
	## Range = 0 suppresses identification of outliers for simple and truncated
	if(type == "tukey")
		resbox <- lapply(x, bxp.stats)
	else
		resbox <- lapply(x, bxp.stats, range=0)
	## Modify resbox to add needed info
	for(i in names(resbox)) {
		target <- resbox[[i]] # make it easy to modify
		target$farout <- numeric(0)
		target$censored <- target$estimated <- -Inf
		target$names <- i
		target$data <- x[[i]]
		if(type == "truncated" && target$n > Box$nobox) { # compute 10/90
			probs <- Box$truncated/100
			tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE)
			target$stats[1,1] <- tobind[1]
			target$stats[5,1] <- tobind[2]
		}
		else if(type == "extended" && target$n > Box$nobox) { # compute 10/90
			probs <- Box$truncated/100
			tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE)
			target$stats[1,1] <- tobind[1]
			target$stats[5,1] <- tobind[2]
			target$out <- x[[i]][x[[i]] < tobind[1] | x[[i]] > tobind[2]]
		}
		else if(type == "tukey" && target$n > Box$nobox) { # any far outs?
			farfence <- 3 * (target$stats[4,1] - target$stats[2,1])
			farout <- (target$out > target$stats[4,1] + farfence) |
				(target$out < target$stats[2,1] - farfence)
			if(any(farout)) { # put into the correct spot
				target$farout <- target$out[farout]
				target$out <- target$out[!farout]
			}
		}
		if(target$n <= Box$nobox) { # suppress box, plot points
			target$out <- target$data[!is.na(target$data)]
			target$stats[seq(5), 1] <- target$stats[3,1]
		}
		## Replace the original
		resbox[[i]] <- target 
	}
	## End of data processing
	resbox$data.range <- c(minx, maxx)
	z <- explain[[type]]
	z$fill <- Box$fill
	return(list(boxes=resbox, explan=explan, z=z))
}
