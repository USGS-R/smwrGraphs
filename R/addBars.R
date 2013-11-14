# Add vertical bars
#
# Coding history:
#    2012Nov11 DLLorenz Original Coding
#    2012Nov11          This version.
#

addBars <- function(x, y, base=0,
                    Bars=list(name="Auto", fill="gray80", outline="black",
                      width="Auto", orientation="stack"),
                    current=list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE)) {
  x <- numericData(x)
  y <- as.matrix(y)
  base <- rep(base, length.out=length(x))
  Bars <- setDefaults(Bars, name="Auto", fill="gray80", outline="black",
                      width="Auto", orientation="stack")
  if(ncol(y) == 1) {
    if(Bars$width == "Auto")
      xoff <- 1/3
    else
      xoff <- Bars$width/2
    ybar <- transData(y[,1], current$yaxis.log, current$yaxis.rev,
                      current$ytrans, current$ytarg)
    rect(x - xoff, base, x + xoff, ybar, col=Bars$fill,
         border=Bars$border, lwd=lineWt("standard"))
    if(Bars$name == "Auto") {
      if(is.null(colnames(y)))
        name <- ""
      else
        name <- colnames(y)
    }
    else
      name <- Bars$name
    ## Need to create a function that will set up the explanation correctly
    Plot <- setPlot(list(), name=name, what='points', type='solid',
                    width='standard', symbol='none', filled=TRUE,
                    size=0.09, color='black', area.color=Bars$fill,
                    area.border=Bars$outline) # force defaults if not set
    explan <- setExplan(Plot, old=current$explanation) # add info to set up explan
  }
  else
    stop("Version dated 11/11 does not support multiple columns")
  current$x <- x
  current$y <- y
  current$explanation <- explan
  invisible(current)
}

