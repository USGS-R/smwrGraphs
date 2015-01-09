#' Print Objects
#' 
#' Print the layout of a figure.
#' 
#' @param x the object to print
#' @param \dots not used, required for other methods
#' @return The object \code{x} is returned invisibly.
#' @note The layout of graphs is displayed.
#' @seealso \code{\link{setLayout}}, \code{\link{setGraph}}
#' @export 
#' @method print Layout
print.Layout <- function(x, ...) {
	mat <- x$mat
	gc <- attr(mat, "graphcount")
	expl <- as.character(gc + 1L)
	cmat <- as.character(mat)
	cmat[cmat == "0"] <- "."
	cmat[cmat == expl] <- "e"
	dim(cmat) <- dim(mat)
	cat("Graph numbers 1 through ", gc, "\nExplanation is e\nGrid cell not available is .\n\n")
	for(i in seq(nrow(cmat)))
		cat(" ", cmat[i,], "\n", sep="")
	cat("\n")
	invisible(x)
}