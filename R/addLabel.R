# add specialized x-axis labels
#
# Coding History:
#    2012Feb15 DLLorenz Original coding
#    2012Sep18 DLLorenz Added long integers
#    2012Nov14 DLLorenz Bug Fix
#    2012Sep18          This version.
#

addLabel <- function(label, x, side="bottom", size="Auto", distance=0.2,
                   justification="center", orientation="parallel",
                   current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                     xaxis.log=FALSE)) { # current plot parameters 
  ## Arguments:
  ##  label (character scalar) the label text or expression
  ##  x (numeric scalar) the x-axis placement of label
  ##  side (character scalar) whihc side? "bottom" or "top"
  ##  size (scale) the size of the text in points
  ##  distance (numeric scalar) the distance from the axis to the text
  ##   not in inches, but relative to text size
  ##  justification - the justification of the label from the point
  ##  orientation - "parallel" or "perpendicular" to the axis
  ##  current - the current plot controls
  ##
  ## convert to usr units
  side <- pmatch(side, c("bottom", "left", "top", "right"))
  if(side %in% c(1L, 3L))
    x <- transData(x, current$xaxis.log, FALSE,
                   current$xtrans, current$xtarg)
  else
    x <- transData(x, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  ## If a size is specified, then change the text size
  if(is.numeric(size)) {
    on.exit(par(cex=1)) # restore default text size
    csi <- par("csi") / 1.2 # converts to actual size of character inches
    par(cex = size/72 / csi)
  }
  orientation <- (pmatch(orientation, c("parallel", "perpendicular")) - 1L) * 2L
  justification <- (pmatch(justification, c("left", "center", "right"),
                           nomatch=1L) - 1)/2
  ## adjust away from the bottom/right if the height is greater than 1 line
  if(orientation == 0L && side %in% c(1L, 4L)) {
    lineSize <- par("cin")[2L]
    textSize <- strheight("M", units="in")
    labelSize <- strheight(label, units="in")
    distance <- distance + (labelSize - textSize) / lineSize
  }
  mtext(label, side=side, line=distance, at=x, adj=justification,
        family="USGS", las=orientation)
  invisible()
}
