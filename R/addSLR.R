#' Add a Regression Line
#' 
#' Computes a simple linear regression from x and y data and adds the line to a graph.
#' 
#' 
#' @aliases addSLR addSLR.default addSLR.list
#' @param x the x-axis data. For method \code{list}, x is a list that contains
#' components \code{x} and \code{y} and the \code{y} argument is not used.
#' Missing values are permitted but ignored.
#' @param y the y-axis data. Missing values are permitted but ignored.
#' @param Plot parameters defining the characteristics of the plot. See
#' \code{\link{setPlot}} for a description of the parameters.
#' @param current the current plot information. Typically, this would be the
#' output from one of the graph creation functions like \code{xyPlot}.
#' @param \dots not used required for other methods.
#' @return The current plot information, the x and y components are the data, not the line.
#'The regression model is included as the lm component.
#' @seealso \code{\link{addXY}}, \code{\link{xyPlot}}
#' @keywords aplot
#' @ addSLR
addSLR <- function(x, y, # data
									 Plot=list(name="", what="lines", type="solid",
									 					width="standard", color="black"), # plot controls
									 Model=list(x="", y="", form="exp", where="none"),
									 current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
									 						 xaxis.log=FALSE),  # current plot parameters
									 ...) { # right-axis labels and titles
	# Coding History:
	#    2014Jul10 DLLorenz Original coding from addSmooth
	#
  UseMethod("addSLR")
}

#' @rdname addSLR
#' @method addSLR default
#' @export
addSLR.default <- function(x, y, # data
													 Plot=list(name="", what="lines", type="solid",
													 					width="standard", color="black"), # plot controls
													 Model=list(x="", y="", form="exp", where="none"),
													 current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
													 						 xaxis.log=FALSE), # current plot parameters
													 ...) { # right-axis labels and titles
	y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- transData(x, current$xaxis.log, FALSE,
                   current$xtrans, current$xtarg)
  ## Remove any missings
  good <- complete.cases(x, y)
	x <- x[good]
	y <- y[good]
  ## Build regression model and equation (if requested)
	Model <- setDefaults(Model, y="", x="", form="exp", where="none")
  lmo <- lm(y ~ x)
	coefs <- coef(lmo)
	if(Model$where == "none") {
		eqn <- ""
	} else if(Model$y == "" | Model$x == "") {
			warning("both x and y must be set in Model")
			eqn <- ""
	} else {
		if(!is.null(current$xtrans) | !is.null(current$ytrans))
			warning("User set transformations ignored in model equation")
		if(Model$form == "exp" & current$xaxis.log & current$yaxis.log) {
			eqn <- as.expression(substitute(widehat(y) == b * x ^ m, 
																			list(y=Model$y, x=Model$x, 
																					 b=signif(10^coefs[1L], 4L),
																					 m=signif(coefs[2L], 4L))))
		} else {
			if(!is.na(current$yaxis.log) && current$yaxis.log)
				y <- paste("log10(", y, ")", sep="")
			if(!is.na(current$xaxis.log) && current$xaxis.log)
				x <- paste("log10(", x, ")", sep="")
			coefs.e <- signif(coefs, 4)
			if(coefs[2] < 0) {
				coefs.e[2] <- -coefs.e[2]
				eqn <- as.expression(substitute(widehat(y) == b - m * x , 
																				list(y=Model$y, x=Model$x, 
																						 b=coefs.e[1L], 
																						 m=coefs.e[2L])))
			} else
				eqn <- as.expression(substitute(widehat(y) == b + m * x , 
																				list(y=Model$y, x=Model$x, 
																						 b=coefs.e[1L],
																						 m=coefs.e[2L])))
		}
	}
  Plot$what <- "lines" # Force lines
  Plot <- setPlot(Plot, name="", what="lines", type="solid",
                  width="standard", color="black") # force defaults if not set
	current <- refLine(coefficients=coefs, Plot=Plot, current=current)
	# Add the equation, if requested
	if(Model$where != "none") {
		xadj <- diff(current$xax$range) * 0.05
		yadj <- diff(current$yax$range) * 0.05
		xpos <- switch(substring(Model$where, 2L),
									 r=current$xax$range[2L] - xadj,
									 c=mean(current$xax$range),
									 l=current$xax$range[1L] + xadj)
		ypos <- switch(substring(Model$where, 1L, 1L),
									 u=current$yax$range[2L] - yadj,
									 c=mean(current$yax$range),
									 l=current$yax$range[1L] + yadj)
		just <- switch(substring(Model$where, 2L),
									 r="right",
									 c="center",
									 l="left")
		addAnnotation(xpos, ypos, eqn, justification=just, position="center")
	}
  current$x <- lmo$x
  current$y <- lmo$y
	current$lm <- lmo
  invisible(current)
}

#' @rdname addSLR
#' @method addSLR list
#' @export
addSLR.list <- function(x, y, # data
												Plot=list(name="", what="lines", type="solid",
																	width="standard", color="black"), # plot controls
												Model=list(x="", y="", form="exp", where="none"),
												current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
																		 xaxis.log=FALSE), # current plot parameters
												...) { # right-axis labels and titles
	xin <- x
  if(is.null(x$y))
    stop("x is a list and must contain the component 'y'")
  y <- x$y
  if(is.null(x$x))
    stop("x is a list and must contain the component 'x'")
  x <- x$x
  if(length(x) != length(y))
    stop("the components 'x' and 'y' must have the same length")
  ## Try to maintain current if input is from a scatter plot
  restore <- FALSE
  if(missing(current) && !is.null(xin$explanation)) {
    xlog <- xin$xaxis.log
    xin$xaxis.log <- FALSE
    yrev <- xin$yaxis.rev
    xin$yaxis.rev <- FALSE
    ylog <- xin$yaxis.log
    xin$yaxis.log <- FALSE
    restore <- TRUE
    current <- xin
  }
	current <- addSLR.default(x, y,	Plot=Plot, Model=Model, current=current)
  if(restore) {
    current$xaxis.log <- xlog
    current$yaxis.rev <- yrev
    current$yaxis.log <- ylog
  }
  invisible(current)
}
