# add title (above graph)
#
# Coding History:
#    2010Nov22 DLLorenz Original coding.
#    2011Oct24 DLLorenz Tweaks for package
#    2013Mar15 DLLorenz Fixed top margin issues.
#

addTitle <- function(Main="", Justification="left", Bold=FALSE) { 
  ## arguments:
  ##   main - the title
  ##   adj - where to plave the title:
  ##         left - on the left side
  ##         center - in the center
  ##         right - on the right side
  ##
  ## get the margin for side 3 (top)
  TopMar <- par("mar")[3]
  if(TopMar < 0.999)
    cat("Not enough room for title!\n")
  else {
    just=(pmatch(Justification, c("left", "center", "right")) - 1)/2
    mtext(text=Main, side=3, line=0, adj=just, font=1 + Bold,
          family="USGS")
  }
  invisible()
}
