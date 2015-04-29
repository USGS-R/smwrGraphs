#' Add a Title
#' 
#' Adds a title (also called heading) to a graph.
#' 
#' If only \code{Heading} is non blank, then the title is a single letter in
#' bold italics. If both \code{Heading} and \code{Main} are non blank, then the
#' title is a single letter followed by a period in bold italics followed by
#' \code{Main} in bold if \code{Bold} is \code{TRUE}.
#' 
#' @param Main the main text of the title.
#' @param Heading The title heading, generally a single letter. See
#' \bold{Details}
#' @param Justification specifies the location of the title, must be one of
#' "left," "center," or "right."
#' @param Bold logical, if \code{TRUE}, then display the title be in bold face type.
#' @return Nothing is returned.
#' @seealso \code{\link{addCaption}}, \code{\link{addAnnotation}},
#' \code{\link{addTable}}
#' @keywords aplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- rnorm(32)
#' Y <- X + rnorm(32)
#' AA.pl <- xyPlot(X, Y)
#' addTitle("X and Y")
#' # For more details of addTitle see
#' vignette(topic="BoxPlots", package="smwrGraphs")
#' vignette(topic="GraphGallery", package="smwrGraphs")
#' vignette(topic="GraphSetup", package="smwrGraphs")
#' }
#' @export addTitle
addTitle <- function(Main="", Heading="", Justification="left", Bold=TRUE) { 
	# Coding History:
	#    2010Nov22 DLLorenz Original coding.
	#    2011Oct24 DLLorenz Tweaks for package
	#    2013Mar15 DLLorenz Fixed top margin issues.
	#    2014Jun25 DLLorenz Converted to roxygen
	##
	## get the margin for side 3 (top)
	TopMar <- par("mar")[3L]
	if(TopMar < 1.49)
		cat("Not enough room for title!\n")
	else {
		just=(pmatch(Justification, c("left", "center", "right")) - 1)/2
		xadj <- strwidth(expression(bold(" ")), family="USGS", cex=9/8, units="user")
		if(Heading != "") {
			# Insert letter heading
			if(Main == "") {
				Heading <- paste(" ", Heading, sep="")
			} else
				Heading <- paste(" ", Heading, ".", sep="")
			# Convert to expression
			Heading <- as.expression(substitute(bolditalic(x), list(x=Heading)))
			mtext(text=Heading, side=3L, line=0.2, adj=just, font=1L,
						family="USGS", cex=9/8)
			xat <- par("usr")[1L] + xadj + strwidth(Heading, family="USGS", cex=9/8, units="user")
		} else
			xat <- par("usr")[1L] + xadj
		if(just > 0) # Subvert computations to place title where user wants
			xat <- NA
		mtext(text=Main, side=3L, at=xat, line=0.2, adj=just, font=1L + Bold,
					family="USGS", cex=9/8)
	}
	invisible()
}
