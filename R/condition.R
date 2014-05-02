# condition a plot
#
# Coding history:
#    2012Nov01 DLLorenz Original scratch idea
#    2013Mar29 DLLorenz Added to package
#

condition <- function(plot.call, data, group, format="grid",
                      num.cols=NA, num.rows=NA , explanation=NULL,
                      yleft=3.5, group.name="",
                      xtitle="", ytitle="", caption="") {
  ## Arguments:
  ##  plot.call (expression using USGS graphics calls)
  ##  data (data.frame) the dataset to use as a source for the variables
  ##     in plot.call and group
  ##  group (factor) how to split the data for each plot.call
  ##  format (character) how to arrange the graphs:
  ##   "table" start in ul
  ##   "grid" start in ll
  ##  num.cols, num.rows, only one should be specified, the other
  ##   inferred from the number of graphs (levels in group)
  ##  explanation, how to set up the explanation
  ##
  ## Get the group data
  ## This works if group is expressed as a name
  group <- eval(group, envir=data)
  if(length(group) == 1) # treat as character and extract from data
    group <- data[[group]]
  if(is.factor(group))
    gps <- levels(group[, drop=TRUE])
  else
    gps <- unique(group)
  format <- match.arg(format, c("table", "grid"))
  N <- length(gps)
  if(is.na(num.cols) && is.na(num.rows)) { # set both to 1
    num.cols <- 1L
    num.rows <- 1L
  }
  else if(is.na(num.cols) && !is.na(num.rows)) # make enough for 1 page
    num.cols <- (N + num.rows - 1) %/% num.rows
  else if(!is.na(num.cols) && is.na(num.rows)) # ditto
    num.rows <- (N + num.cols - 1) %/% num.cols
  GrMax <- num.cols * num.rows
  shx <- 1.1
  if(num.rows == 1L)
    shx <- -1
  shy <- 1.1
  if(num.cols == 1L)
    shy <- -1
  Grkey <- GrMax - num.cols + 1L
  ToDo <- 0L
  Fig <- par("fig")
  for(i in gps) {
    if(ToDo == 0L) { # need to set up layout
      AA.lo <- setLayout(num.rows=num.rows, num.cols=num.cols,
                         shared.x=shx, shared.y=shy, yleft=yleft,
                         explanation=explanation, xtop=1.0)
      ToDo <- 1L
    }
    if(format == "table")
      grd <- ToDo
    else
      grd <- (((GrMax + num.cols) - ToDo) %/% num.cols - 1) * num.cols +
        (ToDo + num.cols - 1) %% num.cols + 1
    AA.gr <- setGraph(grd, AA.lo)
    assign(".margin", AA.gr, pos=1)
    retval <- eval(substitute(plot.call), envir=data[group == i, ],
         enclos=.GlobalEnv)
    ## Add a graph title by brute force
    mtext(paste(group.name, i, sep=""), side=3, line=0.3, adj=0, family="USGS", cex=9/8)
    ## Add the caption if ToDo is 1, and add titles if requested
    if(grd == Grkey) {
      addCaption(caption)
      if(xtitle != "" || ytitle != "") {
        ## Need to make sure that the base line for the figure is OK
        Fig1 <- Fig
        Fig1[3L] <- par("fig")[3L]
        par(fig=Fig1, mar=c(AA.gr[1:2], 1, .5), # captures the upper and right
            usr=c(0,1,0,1))
        if(xtitle != "")
          mtext(text=xtitle, side=1, line=AA.gr[1L] - 2.1, adj=0.5,
                at=0.5, family="USGS", cex=9/8)
        if(ytitle != "")
          mtext(text=ytitle, side=2, line=AA.gr[2L] - 1.75, las=0, adj=0.5,
                at=0.5, family="USGS", cex=9/8)
      }
    }
    ToDo <- ToDo + 1L
    if(ToDo > GrMax)
      ToDo <- 0L
  }
  if(!is.null(explanation))
    setGraph("explanation", AA.lo)
  invisible(retval)
}
