#' Graph Margins
#' 
#' Sets the margins for the plot area (support function).
#' 
#' 
#' @param margin incomplete plot margin specification, generally computed by
#' \code{setGraph}.
#' @param yax the y-axis information from a "pretty" function, required if the
#' second entry of \code{margin} is \code{NA}.
#' @param aux.label logical, if \code{TRUE}, then allocate space for a second 
#' level of x-axis labels.
#' @param caption logical, if \code{TRUE}, then allocate space for figure caption?
#' @return Complete plot margin specification.
#' @seealso \code{\link{setPage}}, \code{\link{setGraph}},
#' \code{\link{setLayout}}
#' @keywords dplot
#' @export setMargin
setMargin <- function(margin, yax, aux.label=FALSE, caption=TRUE) {
	# Coding History:
	#    2008Jun10 DLLorenz Original coding.
	#    2010Nov16 DLLorenz Modified for R (tweaks only)
	#    2011Oct24 DLLorenz Tweaks for package
	#    2012Dec04 DLLorenz Changed default top margin to 1.5 
	#    2014Jun26 DLLorenz Converted to roxygen.
  ##
  if(is.na(margin[4])) { # right margin not set
    margin[4] <- 0.5
    right <- list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE)
  }
  else if(margin[4] < 0) { # suppress ticks
    if(margin[4] < -100)
      margin[4] <- 0
    else
      margin[4] <- abs(margin[4])
    right <- list(ticks=FALSE, labels=FALSE, grid=FALSE, finegrid=FALSE)
  }
  else # margin is set--plot ticks, but not labels
    right <- list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE)
  ##
  if(is.na(margin[3])) { # top margin not set
    margin[3] <- 1.5
    top <- list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE, angle=0)
  }
  else if(margin[3] < 0) { # suppress ticks
    if(margin[3] < -100)
      margin[3] <- 0
    else
      margin[3] <- abs(margin[3])
    top <- list(ticks=FALSE, labels=FALSE, grid=FALSE, finegrid=FALSE, angle=0)
  }
  else # margin is set--plot ticks, but not labels
    top <- list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE)
  ##
  if(is.na(margin[2])) { # left margin not set
    margin[2] <- yax$margin
    left <- list(ticks=TRUE, labels=TRUE, grid=FALSE, finegrid=FALSE)
  }
  else if(margin[2] < 0) { # suppress ticks
    if(margin[2] < -100)
      margin[2] <- 0
    else
      margin[2] <- abs(margin[2])
    left <- list(ticks=FALSE, labels=FALSE, grid=FALSE, finegrid=FALSE)
  }
  else if(margin[2] < 3) # suppress labels, but not ticks
    left <- list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE)
  else # margin is set to allow labels --plot ticks, and labels
    left <- list(ticks=TRUE, labels=TRUE, grid=FALSE, finegrid=FALSE)
  if(is.na(margin[1])) { # bottom margin not set 
    margin[1] <- 4.2 - as.double(!aux.label) - as.double(!caption)
    bot <- list(ticks=TRUE, labels=TRUE, grid=FALSE, finegrid=FALSE, angle=0)
  }
  else if(margin[1] < 0) { # suppress ticks
    if(margin[1] < -100)
      margin[1] <- 0
    else
      margin[1] <- abs(margin[1])
    bot <- list(ticks=FALSE, labels=FALSE, grid=FALSE, finegrid=FALSE, angle=0)
  }
  else if(margin[1] < 2) # supress labels, but not ticks
    bot <- list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE, angle=0)
  else
    bot <- list(ticks=TRUE, labels=TRUE, grid=FALSE, finegrid=FALSE, angle=0)
  return(list(margin=margin, right=right, top=top, left=left, bot=bot))
}
