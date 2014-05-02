# label the axes
#
# Coding History:
#    2007Apr10 DLLorenz Original coding.
#    2008May06 DLLorenz Begin Tweaks 
#    2010Nov19 DLLorenz Modified for R
#    2011Oct24 DLLorenz Tweaks for package
#    2013Mar29 DLLorenz Suppress axis title if inside
#    2014Apr21 DLLorenz level 1 labels 7 pt, level 2 8 pt and titles 9 pt
#

ticks.render <- function(arg1, side, lwd)
  axis(side=side, at=arg1$at, labels=FALSE, tick=TRUE, line=NA, lwd=0,
       lwd.ticks=lwd, tck=arg1$in.length, family="USGS")

renderY <- function(pretty, left=list(ticks=TRUE, labels=TRUE, grid=FALSE,
                              finegrid=FALSE),
                    right=list(ticks=TRUE, labels=FALSE, grid=FALSE,
                      finegrid=FALSE),
                    lefttitle="Y-AXIS TITLE", righttitle="") {
  ## designed for linear, log, and trans axis
  ## set tick length = .5 pica or 0.08 inch and lineweight
  plotsize <- par("pin")
  ticklen <- .08/min(plotsize)
  gridlen <- max(1, plotsize[1L]/plotsize[2L])
  lwd <- frameWt() # frame line width
  if(is.null(left$extend) || !left$extend) {
    minorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
    majorTicks <- pretty$labelpos[!(pretty$labelpos %in% pretty$range)]
  } else { # left$extend must be true
    minorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
    majorTicks <- pretty$labelpos
    ## Draw the axis line
    segments(x0=par("usr")[1L], y0=pretty$labelpos[1L],
             y1=pretty$labelpos[length(pretty$labelpos)],
             lwd=lwd)
  }
  if(!is.null(right$extend) && !right$extend)
    segments(x0=par("usr")[2L], y0=pretty$labelpos[1L],
             y1=pretty$labelpos[length(pretty$labelpos)],
             lwd=lwd)
  ## left-hand axis--use low-level functions
  if(!is.null(left$ticks) && left$ticks) { # put ticks
    ticks.render(list(at=minorTicks, in.length=ticklen/2, out.length=0), 2,
                 lwd=lwd)
    ticks.render(list(at=majorTicks, in.length=ticklen, out.length=0), 2,
                 lwd=lwd)
  }
  par(adj=1)
  labels <- pretty$labels
  if(!is.null(left$labels) && left$labels) # put labels 
    mtext(text=labels, side=2L, at=pretty$labelpos,line=0.2, padj=0.4,
          outer=FALSE, family="USGS", cex=7/8)
  if(!is.null(left$grid) && left$grid) # put gridlines
    ticks.render(list(at=pretty$ticks, in.length=gridlen, out.length=0), 2,
                 lwd=lwd)
  if(!is.null(left$finegrid) && left$finegrid) # put fine gridlines
    ticks.render(list(at=pretty$finegrid, in.length=gridlen, out.length=0), 2,
                 lwd=lwd)
  ## right-hand axis
  if(!is.null(right$ticks) && right$ticks) { # put ticks
    ticks.render(list(at=minorTicks, in.length=ticklen/2, out.length=0), 4,
                 lwd=lwd)
    ticks.render(list(at=majorTicks, in.length=ticklen, out.length=0), 4,
                 lwd=lwd)
  }
  ## NOTE this may require some modification for right-justification of text
  par(adj=0)
  if(!is.null(right$labels) && right$labels) # put labels 
    mtext(text=labels, side=4L, at=pretty$labelpos, line=0.2, padj=0.4,
          outer=FALSE, family="USGS", cex=7/8)
  if(!is.null(right$grid) && right$grid) # put gridlines
    ticks.render(list(at=pretty$ticks, in.length=gridlen, out.length=0), 4)
  if(!is.null(right$finegrid) && right$finegrid) # put fine gridlines
    ticks.render(list(at=pretty$finegrid, in.length=gridlen, out.length=0), 4)
  ##
  par(adj=0.5)
  ## Remember that line=1 offsets for cex=1.0
  ## the line-value for the y-axis label must be based on the width of the labels
  if(!is.character(lefttitle) || lefttitle != "") {
    lineoff <- par("mar")[2L] - 1.7
    if(lineoff > 0)
      mtext(text=lefttitle, side=2L, line=lineoff, padj=0, las=0, family="USGS", cex=9/8)
  }
  if(!is.character(righttitle) || righttitle != "") {
    lineoff <- par("mar")[4L] - 1.7
    if(lineoff > 0)
      mtext(text=righttitle, side=4L, line=lineoff, padj=0, las=0, family="USGS", cex=9/8)
  }
  invisible()
}

renderX <- function(pretty, bottom=list(ticks=TRUE, labels=TRUE, grid=FALSE,
                              finegrid=FALSE, angle=0),
                    top=list(ticks=TRUE, labels=FALSE, grid=FALSE, finegrid=FALSE,
                      angle=0),
                    bottitle="X-AXIS TITLE", toptitle="", 
                    caption="") {
  ## designed for linear, log, trans, date and prob axis
  ## set tick length = .5 pica or 0.08 inch and lineweight
  plotsize <- par("pin")
  ticklen <- .08/min(plotsize)
  gridlen <- max(1, plotsize[1L]/plotsize[2L])
  par(adj=0.5)
  lwd <- frameWt() # frame line width
  if(is.null(bottom$extend) || !bottom$extend) {
    if(pretty$style == "at") {
      minorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
      majorTicks <- pretty$labelpos[!(pretty$labelpos %in% pretty$range)]
      mticklen=ticklen/2
    }
    else {
      minorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
      majorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
      mticklen=0
    }
  }
  else { # bottom$extend must be true
    if(pretty$style == "at") {
      minorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
      majorTicks <- pretty$labelpos
      mticklen=ticklen/2
    }
    else {
      minorTicks <- pretty$ticks[!(pretty$ticks %in% pretty$labelpos)]
      majorTicks <- pretty$ticks
      mticklen=0
    }
    ## Draw the axis line
    segments(x0=pretty$labelpos[1L], y0=par("usr")[3L],
             x1=pretty$labelpos[length(pretty$labelpos)],
             lwd=lwd)
    
  }
  ## Bottom axis--use low-level functions
  if(!is.null(bottom$ticks) && bottom$ticks) { # put ticks
    ticks.render(list(at=minorTicks, in.length=mticklen, out.length=0), 1, lwd=lwd)
    ticks.render(list(at=majorTicks, in.length=ticklen, out.length=0), 1, lwd=lwd)
  }    
  labels <- pretty$labels
  ## Set angle for x-axis labels
  if(!is.null(bottom$angle) && bottom$angle != 0) { # Can only be 0 or 90
    las=2L
  } else
    las=0L
  if(!is.null(bottom$labels) && bottom$labels) { # put labels
  	## Count number of new lines in labels
  	NL <- sapply(gregexpr("\\n", labels), function(m) sum(m > 0))
  	if(las == 2L) # No changes for perpendicular labels
  		NL <- 0
    mtext(text=labels, side=1L, at=pretty$labelpos, line=NL*7/8 + 0.15, 
    			outer=FALSE, family="USGS", las=las, cex=7/8)
  }
  if(!is.null(bottom$grid) && bottom$grid) # put gridlines
    ticks.render(list(at=pretty$ticks, in.length=gridlen, out.length=0L), 1L, lwd=lwd)
  if(!is.null(bottom$finegrid) && bottom$finegrid) # put fine gridlines
    ticks.render(list(at=pretty$finegrid, in.length=gridlen, out.length=0L), 1L,
                 lwd=lwd)
  ## Top axis
  if(!is.null(top$ticks) && top$ticks) { # put ticks
    ticks.render(list(at=minorTicks, in.length=mticklen, out.length=0L), 3L, lwd=lwd)
    ticks.render(list(at=majorTicks, in.length=ticklen, out.length=0L), 3L, lwd=lwd)
  }
  ## NOTE this may require some modification for top-justification of text
  if(!is.null(top$angle) && top$angle != 0)
    par(srt=top$angle, adj=0)
  if(!is.null(top$labels) && top$labels) # put labels
      mtext(text=labels, side=3L, at=pretty$labelpos, line=0.15,
            outer=FALSE, family="USGS", cex=7/8)
  par(srt=0, adj=0.5)
  if(!is.null(top$grid) && top$grid) # put gridlines
    ticks.render(list(at=pretty$ticks, in.length=gridlen, out.length=0), 3L, lwd=lwd)
  if(!is.null(top$finegrid) && top$finegrid) # put fine gridlines
    ticks.render(list(at=pretty$finegrid, in.length=gridlen, out.length=0), 3L,
                 lwd=lwd)
  ##
  ## remember that line=1 offsets for cex=1.0
  ## if label 2 and a request to draw labels, font size is 8
  if(!is.null(pretty$label2pos) && length(pretty$label2pos) > 0) {
    if(!is.null(bottom$labels) && bottom$labels) {
      mtext(text=pretty$label2, side=1L, at=pretty$label2pos,
            line=1.2, family="USGS")
    }
    ## do not draw the separator--let the illustrator do it
  }
  lineoff <- par("mar")[1L] - 2.1
  if(!is.character(bottitle) || bottitle != "")
    if(lineoff > 0)
      mtext(text=bottitle, side=1L, line=lineoff, family="USGS", cex=9/8)
  if(!is.character(toptitle) || toptitle != "")
    if(lineoff > 0)
      mtext(text=toptitle, side=3L, line=1.2, family="USGS", cex=9/8)
  if(!is.character(caption) || caption != "")
    addCaption(caption)
  invisible()
}
