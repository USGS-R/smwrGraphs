# Add error bars
#
# Coding history:
#    2013Sep19 DLLorenz Original Coding
#    2012Sep19          This version.
#

addErrorBars <- function(x, yup, ylo,
                    Bars=list(name="", cap=0.09, width="standard", color="black"),
                    current=list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE)) {
  x <- numericData(x)
  x <- transData(x, current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  yup <- numericData(yup)
  yup <- transData(yup, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  ylo <- numericData(ylo)
  ylo <- transData(ylo, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  Bars <- setDefaults(Bars, name="", cap=0.09, color="black")
  if(Bars$cap > 0) {
    doCaps <- TRUE
    ## Compute the inches per user unit
    pin <- par('pin')
    usr <- par('usr')
    uin <- pin/c(usr[2L] - usr[1L], usr[4L] - usr[3L])
    xoff <- Bars$cap/2/uin[1L]
  } else
    doCaps <- FALSE
  ## Need to create a function that will set up the explanation correctly
  Plot <- setPlot(list(), name=Bars$name, what="points", type="solid",
                  width=Bars$width, symbol="+", filled=FALSE,
                  size=0.09, color=Bars$color, area.color=Bars$fill,
                  area.border=Bars$outline) # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explan
  plotPars <- explan$current
  for(i in seq(along=(x))) {
    segments(x[i], ylo[i], x[i], yup[i], lwd=plotPars$lwd, col=plotPars$col)
    if(doCaps) {
      segments(x[i]-xoff, ylo[i], x[i]+xoff, ylo[i], lwd=plotPars$lwd, col=plotPars$col)
      segments(x[i]-xoff, yup[i], x[i]+xoff, yup[i], lwd=plotPars$lwd, col=plotPars$col)
    }
  }
  ## Fix the symbol for the explanation
  N <- length(explan$lines$pch)
  explan$lines$pch[N] <- 73 # I
  current$explanation <- explan
  invisible(current)
}
