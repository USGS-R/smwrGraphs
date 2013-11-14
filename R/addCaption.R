# add a caption
# Usefull when explanation is at bottom and caption can't be added to plot
#
# Coding History:
#    2009Apr23 DLLorenz Original coding and start of revisions
#    2011May23 DLLorenz Conversion to R
#    2012Sep18 DLLorenz Added long integers
#    2012Sep18          This version.
#

addCaption <- function(caption='') {
  lineoff <- par("mar")[1L]
  par(family='USGS')
  if(is.expression(caption) || caption != '')
    mtext(text=caption, side=1L, line=lineoff - 1, 
          at=par("usr")[1L], adj=0)
  invisible()
}
