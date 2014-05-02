# add minor ticks to linear axes
#
# Coding History:
#    2014Jan31 DLLorenz Original coding.
#

addMinorTicks <- function(which, current, ticks) {
  ## Arguments:
  ##  which (character scalar) which axis to label
  ##  current (output from primary plotting function)
  ##  ticks (logical scalar) draw the ticks or numeric--
  ##  Number of ticks per interval
  which <- match.arg(which, c("bottom", "left", "top", "right", "x", "y"))
  plotsize <- par("pin")
  ticklen <- .04/min(plotsize)
  lwd <- frameWt()
	if(which %in% c("bottom", "top", "x")) {
		ax <- current$xax
	} else
		ax <- current$yax
  at <- ax$finegrid[!(ax$finegrid %in% ax$ticks)]
  if(!missing(ticks)) {
  	# Logic is not exaclty straightforward, but easy coding
  	at <- apply(cbind(ax$ticks[-length(ax$ticks)], ax$ticks[-1L]),1L,
  							function(x) cumsum(rep(diff(x)/(ticks+1), ticks))+x[1])
  	at <- as.vector(at) # strip matrix attributes
  }
  if(which %in% c("bottom", "x")) {
  	axis(side=1L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  if(which %in% c("top", "x")) {
  	axis(side=3L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  if(which %in% c("left", "y")) {
  	axis(side=2L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  if(which %in% c("right", "y")) { 
  	axis(side=4L, at=at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
  			 lwd.ticks=lwd, tck=ticklen, family="USGS")
  }
  invisible(current)
}
