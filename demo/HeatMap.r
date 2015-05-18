library(smwrGraphs)
# Create the dummy dataset for this example
PP <- structure(list(a1 = c(0.5, 0.93, 0.31, 4.36, 2.1, 2.42, 4.5, 2.7), 
  b2 = c(1.74, 1.9, 0.77, 2.17, 0.03, 3.49, 1.17, 1.23), 
  c3 = c(1.47, 2.88, NA, 4.7, 3.15, 2.5, 0.77, 2.45), 
  d4 = c(4.66, 4.39, 3.09, 1.94, 4.46, 3.86, 3.47, 0.3)), 
  .Names = c("a1", "b2", "c3", "d4"), 
  row.names = c("AA", "BB", "CC", "DD", "EE",  "FF", "GG", "HH"), 
  class = "data.frame")
# Set up the data and clustering
# The data, include any scaling, etc.
tmp.mat <- data.matrix(PP)
# The description of the data (for explanation)
tmp.nam <- "Random Data"
# Note may want to preserve the clusters for future use
# The row (observation) clusters, specify method, etc
tmp.rcl <- hclust(dist(tmp.mat), method="complete")
# The column (variable clusters, specify method, etc
tmp.ccl <- hclust(dist(t(tmp.mat)), method="ave")
# Now some other stuff for the graph
# The cell size (inches), should be greater than .111 (1/9 inch)
tmp.size <- .5
# The width (inches) of the row dendrogram
tmp.rdd <- 1.5
# The height (inches) of the column dendrogram
tmp.cdd <- 1.5
# The width of the explanation, 0 for no explanation; should be just larger than 1 inch
tmp.exp <- 1.1
# The colors, specify ramp (and number of colors)
#tmp.col <- coolWarm.colors(5) # typical color ramp
tmp.col <- c("gray", "yellow", "orange", "orange3", "red")
# And the color range breaks for the data in tmp.mat, 
# 2 steps in this version, can also specify the exact breaks
tmp.brk <- range(pretty(tmp.mat))
tmp.brk <- seq(tmp.brk[1L], tmp.brk[2L], length.out=length(tmp.col)+1L)
# Clean up? (remove the tmp. objects from this script)
tmp.clup <- TRUE
# The output file name for PDF; if blank, then on screen
tmp.fil <- "HeatMap"
#---------------------------------------------------------
## OK, lets go
# Estimate width and height of the box
tmp.wid <- ncol(tmp.mat)*tmp.size
tmp.hgt <- nrow(tmp.mat)*tmp.size
# And the width and height of the margins
tmp.rmr <- max(nchar(rownames(tmp.mat)))/7.5
tmp.cmr <- max(nchar(colnames(tmp.mat)))/7.5
if(tmp.fil == "") {
  setPage(list(height=1+tmp.cdd+tmp.hgt+tmp.cmr,
               width=1+tmp.rdd+tmp.wid+tmp.rmr+tmp.exp),
          font="USGS", device="windows")
} else {
  setPDF(list(height=1+tmp.cdd+tmp.hgt+tmp.cmr,
              width=1+tmp.rdd+tmp.wid+tmp.rmr+tmp.exp),
         basename=tmp.fil)
}
if(tmp.exp > 0) {
  tmp.lo <- setLayout(width=c(tmp.rdd, tmp.wid+tmp.rmr),
                      height=c(tmp.cdd,tmp.hgt+tmp.cmr),
                      explanation=list(right=tmp.exp))
} else {
  tmp.lo <- setLayout(width=c(tmp.rdd, tmp.wid+tmp.rmr),
                      height=c(tmp.cdd,tmp.hgt+tmp.cmr))
}
# OK, do the dendrograms
tmp.cgm <- as.dendrogram(tmp.ccl)
# As it happens the unlisted dendrogram is the order of the colmns or rows
tmp.mat <- tmp.mat[, unlist(tmp.cgm)]
setGraph(2, tmp.lo)
par(mai=c(0,0,0, tmp.rmr))
plot(tmp.cgm, leaflab="none", edgePar=list(lwd=stdWt()), 
     xlim=c(0, ncol(tmp.mat)) + .5,  yaxt="n",
     ylim=c(0, round(attr(tmp.cgm, "height") + .1,1)))
addLabel(expression("Increasing Difference between Variables" %->% ""), 
         round(attr(tmp.cgm, "height") + .1,1)/2, side="right")
# And the rows
tmp.rgm <- as.dendrogram(tmp.rcl)
tmp.mat <- tmp.mat[unlist(tmp.rgm),]
setGraph(3, tmp.lo)
par(mai=c(tmp.cmr,0,0,0))
plot(tmp.rgm, leaflab="none", edgePar=list(lwd=stdWt()), horiz=TRUE,
     ylim=c(0, nrow(tmp.mat)) + .5,  yaxt="n", yaxs="i",
     xlim=c(round(attr(tmp.rgm, "height") + .1,1), 0))
addLabel(expression("" %<-% "Increasing Difference between Samples"),
         round(attr(tmp.rgm, "height") + .1,1)/2)
# Now the image
setGraph(4, tmp.lo)
plot.new() # required to set up for the call to image
par(mai=c(tmp.cmr,0,0, tmp.rmr), usr=c(0, ncol(tmp.mat), 0, nrow(tmp.mat)) + .5)
image(seq(ncol(tmp.mat)), seq(nrow(tmp.mat)),
      t(tmp.mat), breaks=tmp.brk, add=TRUE, col=tmp.col)
box(, lwd=lineWt("hairline"))
# Add grid lines
for(i in seq(nrow(tmp.mat) - 1) + .5){
  segments(0.5, i, ncol(tmp.mat) + .5, i, lwd=lineWt("hairline"))
}
for(i in seq(ncol(tmp.mat) - 1) + .5){
  segments(i, 0.5, i, nrow(tmp.mat) + .5, lwd=lineWt("hairline"))
}
# Add the Labels
mtext(rownames(tmp.mat), side=4, at=seq(nrow(tmp.mat)), family="USGS", 
      line=0.2, adj=0)
mtext(colnames(tmp.mat), side=1, at=seq(ncol(tmp.mat)), family="USGS", 
      line=0.1, adj=1, las=2)
# Create and add explanation
if(tmp.exp > 0) {
  # These are the required components for an color ramp-like explanation
  tmp.cnt <- list(zvalues=matrix((tmp.brk[-length(tmp.brk)] + tmp.brk[-1L])/2, nrow=1),
                  fillcol=tmp.col, breaks=tmp.brk, xvals=c(.1, .35),
                  yvals=seq(3.0, 0.5, length.out=length(tmp.brk)),
                  linecol="black", name=tmp.nam, linewt=lineWt("hairline"))
  setGraph("explanation", tmp.lo)
  addExplanation(list(explanation=list(contour=tmp.cnt)), 
                 margin=c(0, 0, tmp.cdd*7.5, 0)) # adjust for columns dendrogram
}
if(tmp.fil != "") {
  dev.off() # close the PDF file
}
if(tmp.clup) {
  rm(tmp.brk,tmp.ccl,tmp.cdd,tmp.cgm,tmp.clup,tmp.cmr,tmp.cnt,tmp.col,
     tmp.exp,tmp.fil,tmp.hgt,tmp.lo,tmp.mat,tmp.nam,tmp.rcl,tmp.rdd,
     tmp.rgm,tmp.rmr,tmp.size,tmp.wid, i)
}