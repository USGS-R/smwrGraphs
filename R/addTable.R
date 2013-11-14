# Add a table (hopefully small) to a graph
#
# Coding history:
#    2009Aug14 DLLorenz Original Coding and start of revisions
#    2011Apr09 DLLorenz Begin modifications for R
#    2011Aug03          This version.
#

addTable <- function(tbl, where='ll', title='') {
  ## Arguments:
  ##  tbl (character matrix or data.frame) the data to add to graph; 
  ##    the matrix has more flexible column naming options.
  ##  where (character scalar) in which corner should the tbale be placed?
  ##  title (character scalar)  the title of the table
  ##
  ## To compute where the point is for locating the table, the user needs 'usr'
  ## Adjust usr so the explanation is not exactly in the corner
  usradj <- par('usr')
  usradj[1] <- usradj[1] + (usradj[2] - usradj[1]) * 0.02
  usradj[2] <- usradj[2] - (usradj[2] - usradj[1]) * 0.02
  usradj[3] <- usradj[3] + (usradj[4] - usradj[3]) * 0.02
  usradj[4] <- usradj[4] - (usradj[4] - usradj[3]) * 0.02
  ## For now, assume explanation goes on the plot
  ## pos is x-y and corner info for the call to legend()
  pos <- switch(where,
                ul=c(usradj[c(1,4)], 0, 1),
                ur=c(usradj[c(2,4)], 1, 1),
                ll=c(usradj[c(1,3)], 0, 0),
                lr=c(usradj[c(2,3)], 1, 0))
  Seq <- switch(where,
                ul=seq(ncol(tbl)),
                ur=rev(seq(ncol(tbl))),
                ll=seq(ncol(tbl)),
                lr=rev(seq(ncol(tbl))))
  Dir <- switch(where,
                ul=1,
                ur=-1,
                ll=1,
                lr=-1) # Accounts for narrow text
  ColNames <- dimnames(tbl)[[2]]
  par(family='USGS')
  for(i in Seq) {
    if(i == 1) # Place the title
      DoTitle <- title
    else
      DoTitle <- ""
    Col <- c(ColNames[i], tbl[,i])
    Off <- legend(pos[1], pos[2], legend=Col, bty='n',
                  xjust=pos[3], yjust=pos[4], title=DoTitle,
                  cex=1)
    ## All units in Off are usr, so adjust accordingly--the last part of
    ## the adjustment shrinks the distance because there is no symbol
    pos[1] <- pos[1] + Dir * (Off$rect$w - (Off$text$x[1] - Off$rect$left) / 1.5)
  }
  invisible()
}
