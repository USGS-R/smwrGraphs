# add axis labels
#
# Coding History:
#    2011Aug03 DLLorenz Original coding.
#    2011Dec28          This version.
#

addAxisLabels <- function(which, current, title="", ticks=FALSE, labels=TRUE) {
  ## Arguments:
  ##  which (character scalar) which axis to label
  ##  current (output from primary plotting function)
  ##  title (character scalar) the axis title
  ##  ticks (logical scalar) draw the ticks
  ##  labels (logical scalar) draw the labels
  ##
  which <- match.arg(which, c("bottom", "left", "top", "right"))
  thisside=list(ticks=ticks, labels=labels, grid=FALSE, finegrid=FALSE, angle=0)
  othside=list(ticks=FALSE, labels=FALSE, grid=FALSE, finegrid=FALSE, angle=0)
  switch(which,
         bottom=renderX(current$xax, bottom=thisside, top=othside, bottitle=title),
         top=renderX(current$xax, bottom=othside, top=thisside, bottitle='',
           toptitle=title),
         left=renderY(current$yax, left=thisside, right=othside, lefttitle=title),
         right=renderY(current$yax, left=othside, right=thisside, lefttitle='',
           righttitle=title))
  invisible(current)
}
