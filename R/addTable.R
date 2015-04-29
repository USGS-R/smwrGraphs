#' Add Table to Graph
#' 
#' Adds a table to a graph.
#' 
#' \code{where} must be one of "ul," "ur," "ll," or "lr." The first letter is
#' an abbreviation for upper or lower, the second letter is an abbreviation for
#' left or right.
#' 
#' @param tbl the data frame or matrix to add to graph. All data must be of
#' mode character, which allows the user to format the data rather than the
#' automatic formatting done by R.
#' @param where character specifying the corner the table should be placed, see
#' \bold{Details}.
#' @param title the title of the table.
#' @return Nothing is returned.
#' @note The table should be formatted by the user for anything other than
#' right justification. The column names of the table are the column names of
#' the matrix or data frame. The matrix gives the user mnore control over
#' column names than does the data frame.
#' @seealso \code{\link{addExplanation}}, \code{\link{addAnnotation}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' AA.pl <- xyPlot(X, Y)
#' Mat <- cbind(c("Mean of X", "Mean of Y"), round(c(mean(X), mean(Y)), 2))
#' addTable(Mat, "ul")
#' # For more details of addTable see
#' vignette(topic="GraphAdditions", package="smwrGraphs")
#' }
#' @export addTable
addTable <- function(tbl, where='ll', title='') {
	# Coding history:
	#    2009Aug14 DLLorenz Original Coding and start of revisions
	#    2011Apr09 DLLorenz Begin modifications for R
	#    2014Jun25 DLLorenz Converted to roxygen
  ##
  ## To compute where the point is for locating the table, the user needs 'usr'
  ## Adjust usr so the explanation is not exactly in the corner
  usradj <- par('usr')
  usradj[1] <- usradj[1] + (usradj[2] - usradj[1]) * 0.02
  usradj[2] <- usradj[2] - (usradj[2] - usradj[1]) * 0.02
  usradj[3] <- usradj[3] + (usradj[4] - usradj[3]) * 0.02
  usradj[4] <- usradj[4] - (usradj[4] - usradj[3]) * 0.02
  ## For now, assume explanation goes on the plot
  ## pos is x-y and corner info for the call to legend()
  pos <- switch(where,
                ul=c(usradj[c(1,4)], 0, 1),
                ur=c(usradj[c(2,4)], 1, 1),
                ll=c(usradj[c(1,3)], 0, 0),
                lr=c(usradj[c(2,3)], 1, 0))
  Seq <- switch(where,
                ul=seq(ncol(tbl)),
                ur=rev(seq(ncol(tbl))),
                ll=seq(ncol(tbl)),
                lr=rev(seq(ncol(tbl))))
  Dir <- switch(where,
                ul=1,
                ur=-1,
                ll=1,
                lr=-1) # Accounts for narrow text
  ColNames <- dimnames(tbl)[[2]]
  par(family='USGS')
  for(i in Seq) {
    if(i == 1) # Place the title
      DoTitle <- title
    else
      DoTitle <- ""
    Col <- c(ColNames[i], tbl[,i])
    Off <- legend(pos[1], pos[2], legend=Col, bty='n',
                  xjust=pos[3], yjust=pos[4], title=DoTitle,
                  cex=1)
    ## All units in Off are usr, so adjust accordingly--the last part of
    ## the adjustment shrinks the distance because there is no symbol
    pos[1] <- pos[1] + Dir * (Off$rect$w - (Off$text$x[1] - Off$rect$left) / 1.5)
  }
  invisible()
}
