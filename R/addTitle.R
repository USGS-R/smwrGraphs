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
#' @param Justification specify the horizontal location of the title, must be one of
#' "left," "center," or "right."
#' @param Bold logical, if \code{TRUE}, then display the title in bold face type.
#' @param Position specify the vertical location of the title, must be either
#' "above" or "inside."
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
addTitle <- function(Main="", Heading="", Justification="left", Bold=TRUE, 
										 Position="above") { 
	##
	Position <- match.arg(Position, c("above", "inside"))
	## get the margin for side 3 (top)
	TopMar <- par("mar")[3L]
	if(TopMar < 1.49 && Position == "above") {
		warning("Not enough room for title!\n")
	} else {
		just <- (pmatch(Justification, c("left", "center", "right")) - 1)/2
		just <- (just - .5)*.98 + .5 # move away from edges
		line <- 1.0
		if(Position == "inside") {
			# Modify line and just
			line <- -0.75
			just <- (just - .5)*.96 + .5 # move away from plot ticks
		}
		if(Heading != "" && Main != "") {
			Heading <- paste(Heading, ".", sep="")
		}
		# Convert to expression
		if(Bold) {
			Heading <- as.expression(substitute(paste(bolditalic(x), " ", bold(y)), 
																					list(x=Heading, y=Main)))
		} else {
			Heading <- as.expression(substitute(paste(italic(x), " ", y), 
																					list(x=Heading, y=Main)))
		}
		mtext(text=Heading, side=3L, line=line, adj=just, font=1L, padj=1,
					family="USGS", cex=9/8)
	}
	invisible()
}
