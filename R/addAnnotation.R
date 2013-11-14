# add text to a plot
#
# Coding History:
#    2008Aug01 DLLorenz Original coding and begin of tweaks.
#    2011Jan20 DLLorenz Conversion to R
#    2011Oct24 DLLorenz Tweaks for package
#    2012Sep18 DLLorenz Added long integers
#    2012Sep18          This version.
#

addAnnotation <- function(x, y, annotation, # data
                          leaderx=NULL, leadery=NULL, leadercol="black", # leader controls
                          angle=0, justification="left", size=8,
                          position="above", # placement control
                          current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                            xaxis.log=FALSE)) { # current plot parameters 
  ## Arguments:
  ##  x (numeric scalar) the x-axis placement of anno
  ##  y (numeric scalar) the y-axis placement of anno
  ##  annotation (character scalar) the text
  ##  leaderx - draw leader from x value to leaderx
  ##  leadery - draw leader from y value to leadery
  ##  leadercol the color fo the leader
  ##  angle - the angle to rotate the text
  ##  justification - the justification of the label from the point
  ##  position - the vertical location of the anno
  ##  size - character size in points
  ##  current - the current plot controls
  ##
  ## convert to usr units
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                 current$ytrans, current$ytarg)
  x <- transData(x, current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  on.exit(par(cex=1)) # restore default text size
  csi <- par("csi") / 1.2 # converts to actual size of character inches
  par(cex = size / 72 / csi)
  position <- match.arg(position, c("above", "below", "center"))
  justification <- (pmatch(justification, c("left", "center", "right"),
                           nomatch=1L) - 1)/2
  ## Compute the inches per user unit
  pin <- par('pin')
  usr <- par('usr')
  uin <- pin/c(usr[2L] - usr[1L], usr[4L] - usr[3L])
  ## A small adjustment is needed to y to adjust for assymetric font/line
  ##  relation.
  ytxt <- y + 0.01/uin[2L]
  xtxt <- x + 0.08/uin[1L] * (0.5 - justification)
  if(position == "above") # Adjust y up
    ytxt <- ytxt + 0.04/uin[2L] + strheight(annotation)/2
  else if(position == "below") # Adjust y down
    ytxt <- ytxt - 0.04/uin[2L] - strheight(annotation)/2
  ## Place the annotation
  text(xtxt, ytxt, annotation, srt=angle, adj=justification, family='USGS')
  if(!is.null(leaderx)) {
    leadery <- transData(leadery, current$yaxis.log, current$yaxis.rev,
                         current$ytrans, current$ytarg)
    leaderx <- transData(leaderx, current$xaxis.log, FALSE,
                         current$xtrans, current$xtarg)
    segments(x, y, leaderx, leadery, col=leadercol, lwd=stdWt())
  }
  invisible(current)
}
