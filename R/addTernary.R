# Adds plots to a Ternary diagram.
#    Calls ternarySubplot
#
# Coding history:
#    2012Nov02 DLLorenz Original coding and revisions.
#    2012Nov02          This version.
#

addTernary <- function(x, y, z,# data (need not sum to 100)
                       Plot=list(name="", what='points', type='solid',
                         width='standard', symbol='circle', filled=TRUE,
                         size=0.09, color='black'), # plot controls
                       current=list()) { # current parameters
  ## Arguments:
  ##  x (numeric vector) and
  ##  y (numeric vector) and
  ##  zt (numeric vector) the data for the x-, y-, and z-axes. Need not sum to
  ##    1 or 100.
  ##  Plot (list) defining the characteristics of the plot
  ##  current (list) the axis parameters for the current plot.
  ## 
  ## Process plot controls
  Plot <- setPlot(Plot, name="", what="points", type="solid",
                  width="standard", symbol="circle", filled=TRUE,
                  size=0.09, color="black")
  explan <- setExplan(Plot, current$explanation)
  ## Normalize the data according to range
  current <- setDefaults(current, axis.range=c(0, 100), orient="c")
  axis.range <- current$axis.range
  tsum <- sumComposition(x, y, z, Range=axis.range[2L])
  Data <- ternarySubplot(tsum[, 1L], tsum[, 2L], tsum[, 3L],
                         axis.range=axis.range, orient=current$orient,
                         plot=FALSE)
  points(Data$x, Data$y, type=explan$current$type,
         lwd=explan$current$lwd, lty=explan$current$lty,
         pch=explan$current$pch, cex=explan$current$cex,
         col=explan$current$col, bg=explan$current$col)
  ## Note that plot info does not pertain here
  retval <- list(x=Data$x, y=Data$y, Data=tsum, orient=current$orient,
                 axis.range=axis.range, explanation=explan)
  invisible(retval)
}
