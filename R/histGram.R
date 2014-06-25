# Create a plot set up to show data on multiple x- or y-axss
#
# Coding history:
#    2012Oct16 DLLorenz Initial coding and begin edits
#    2013Apr09 DLLorenz Added setGD 
#

histGram <- function(x, breaks="Sturges", # data specs
                     Hist=list(), # plot cntrls
                     yaxis.range=c(NA,NA), # y-axis controls
                     ylabels=7, xlabels=7, # labels
                     xtitle="",
                     ytitle="Auto", # axis titles
                     caption="",# caption
                     margin=c(NA, NA, NA, NA), ...) { # margin control
  ## Arguments:
  ##  x (numeric vector or matrix) the data to create the histogram
  ##  breaks, see ?hist
  ##  Hist (list) Components of the histogram, type must be either "frequency" or
  ##   "density," boundary must be either "lower" or "upper." The value for boundary
  ##   indicates the cut point includes values strictly less than ("lower") or
  ##   less than or equal to ("upper").
  ## Note the language amy need revision so that it makes sense to non stats people.
  UseMethod("histGram")
}

histGram.default <- function(x, breaks="Sturges", # data specs
                             Hist=list(type="frequency", fill=FALSE, boundary="lower",
                               line.color="black", fill.color="gray80"), # plot cntrls
                             yaxis.range=c(NA,NA), # y-axis controls
                             ylabels=7, xlabels=7, # labels
                             xtitle=deparse(substitute(x)),
                             ytitle="Auto", # axis titles
                             caption="",# caption
                             margin=c(NA, NA, NA, NA), ...) { 
  ## Note, to set the x-axis range, set the specific breaks
  xtitle <- xtitle
  if(dev.cur() == 1)
    setGD("Histogram")
  ## Need to figure out how to process xaxis.log, maybe just log(x)
  ## But then need to find nice labels for x too!
  Hist <- setDefaults(Hist, type="frequency", fill=FALSE, boundary="lower",
                      line.color="black", fill.color="gray80")
  Hist$type <- match.arg(Hist$type, c("frequency", "density"))
  Hist$boundary <- match.arg(Hist$boundary, c("lower", "upper"))
  ret <- hist(x, breaks, right=Hist$boundary == "upper", plot=FALSE)
  xax <- linearPretty(range(ret$breaks), TRUE, xlabels)
  if(Hist$type == "frequency") {
    if(any(is.na(yaxis.range)))
      yax <- linearPretty(c(0, max(ret$counts)), FALSE, ylabels, extend.range=FALSE)
    else
      yax <- linearPretty(yaxis.range, TRUE, ylabels)
    freq <- TRUE
    if(ytitle == "Auto")
      ytitle <- "Frequency"
  }
  else {
    if(any(is.na(yaxis.range)))
      yax <- linearPretty(c(0, max(ret$density)), FALSE, ylabels, extend.range=FALSE)
    else
      yax <- linearPretty(yaxis.range, TRUE, ylabels)
    freq <- FALSE
    if(ytitle == "Auto")
      ytitle <- "Density"
  }
  ## Set margins, controls in calls to render
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  par(mar=margin)
  fillcolor <- if(Hist$fill) Hist$fill.color
  plot(ret, freq=freq, col=fillcolor, border=Hist$line.color,
       main="", sub="", xlab="", ylab="", xlim=xax$range, ylim=yax$range,
       axes=FALSE, labels=FALSE)
  ## Set up the explanation
  Col <- if(Hist$fill) Hist$fill.color else "white"
  Plot <- setPlot(list(), name=ytitle, what='points', type='solid',
                  width='standard', symbol='none', filled=TRUE,
                  size=0.09, color='black', area.color=Col,
                  area.border=Hist$line.color) # force defaults if not set
  explan <- setExplan(Plot, old=list()) # add info to set up explanation
  ## Label the axes No box! and need extend=TRUE
  renderY(yax, lefttitle=ytitle, left=list(ticks = TRUE, labels = TRUE,
                                   grid = FALSE, finegrid = FALSE,
                                   extend=TRUE),
          right=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
            finegrid = FALSE))
  renderX(xax, bottitle=xtitle, bottom=list(ticks = TRUE, labels = TRUE,
                                  grid = FALSE, finegrid = FALSE,
                                  extend = TRUE),
          top=list(ticks = FALSE, labels = FALSE, grid = FALSE, 
            finegrid = FALSE), caption=caption)
  ## Need to complete the info needed in ret
  invisible(list(hist=ret, yaxis.log=FALSE, yaxis.rev=FALSE,
                  xaxis.log=FALSE, explanation=explan, margin=margin,
                  yax=yax, xax=xax))
}
