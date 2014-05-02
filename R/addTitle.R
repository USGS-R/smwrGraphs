# add title (above graph)
#
# Coding History:
#    2010Nov22 DLLorenz Original coding.
#    2011Oct24 DLLorenz Tweaks for package
#    2013Mar15 DLLorenz Fixed top margin issues.
#

addTitle <- function(Main="", Heading="", Justification="left", Bold=TRUE) { 
	## arguments:
	##   main - the title
	##    heading - the letter heading
	##   adj - where to place the title:
	##         left - on the left side
	##         center - in the center
	##         right - on the right side
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
