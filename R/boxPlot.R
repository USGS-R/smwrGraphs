# USGS style boxplot function.
#
# Coding history:
#    2008May15 DLLorenz Modified from usgsboxplot to produce 'pub' ready plots
#    2010Dec16 DLLorenz Conversion to R
#    2011Jun25 DLLorenz Interface update and fix rotation of x-labels
#    2011Aug03 DLLorenz Added axis labeling info to current
#    2011Oct24 DLLorenz Bug fix and Tweaks for package
#    2011Dec16 DLLorenz Conversion to generic function with methods
#    2012Feb24 DLLorenz Begin update to new pub standards
#    2012Apr20 DLLorenz Created fcn to compute quartiles, not hinges
#    2012Aug31 DLLorenz Big fixes and tweaks
#    2012Sep18 DLLorenz Added fill option to box.
#    2012Nov01 DLLorenz Bug fix on missing values
#    2013Jan13 DLLorenz Change drop censor args in Box (retain in defaults)
#    2013Apr10 DLLorenz Added setGD
#    2014Jan28 DLLorenz Added match.arg for Box
#    2014Apr23 DLLorenz Bug fix for NAs

boxPlot <- function(..., group=NULL, # data
                    Box=list(type="truncated", show.counts=TRUE, 
                      nobox=5, width="Auto", fill="none",
                      truncated=c(10,90)), # b&w controls, color always black!
                    yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                    ylabels="Auto", xlabels="Auto", xlabels.rotate=FALSE, # labels
                    xtitle="", ytitle="",  caption="", # axis titles and caption
                    margin=c(NA,NA,NA,NA)) { # margin control
  ## Arguments:
  ##  ... (various) the data to plot
  ##  group (any vector) creatre groups of a single vector to plot ignored
  ##    if ... contains more than one entry or entry has
  ##    structure (is a list or data.frame)
  ##  Box (list) paramters for the box
  ##
  ##  xlabels.rotate (logical scalar) rotate x-axis labels 90 degrees or not (0)
  UseMethod("boxPlot")
}

## Compute the stats for a normal (no censoring) boxplot
boxPlotStats <- function(x, Box, yaxis.log) {
  ## Arguments:
  ##  x (tagged list) the raw data
  ##  Box (tagged list) the type of boxplot, etc, must be fully populated before call
  ##  yaxis.log (logical scalar) log-tranform the data?
  ##
  type <- match.arg(Box$type, c("tukey", "truncated", "simple", "extended"))
  ## The text to use for the explanation:
  explain <- list(tukey=list(z=list(stats=matrix(c(-1.2, -0.1,.8, 1.5, 3.6),
                                      ncol=1),
                               n=54, names="",
                               out=-2.6, farout=-3.9,
                               group=c(1, 1)),
                    labels=list(expression(bold("Number of values")),
                      list(expression(bold("Largest value within 1.5 times")),
                           expression(bold("  interquartile range above")),
                           expression(bold("  75th percentile"))),
                      "75th percentile",
                      "50th percentile\n  (median)",
                      "25th percentile",
                      list(expression(bold("Smallest value within 1.5 times")),
                           expression(bold("  interquartile range below")),
                           expression(bold("  25th percentile"))),
                      list(expression(paste(bold("Outside value"), symbol("\276"),
                          "Value is > 1.5 and", sep='')),
                           " < 3  times the interquartile range",
                           "  beyond either end of box"),
                      list(expression(paste(bold("Far-out value"),symbol("\276"),
                          "Value ", is >= 3, " times", sep='')),
                           "  the interquartile range beyond", "  either end of box")),
                    values=c(4.5, 3.6, 1.5, .7, -0.1, -1.2, -2.6, -3.9),
                    IQR=expression(bold("Interquartile\nrange"))),
                  truncated=list(z=list(stats=matrix(c(-3.0, -1.2 ,0, 1.5, 3.5),
                                          ncol=1),
                                   n=54, names=""),
                    labels=list(expression(bold("Number of values")),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[2], "th percentile", sep='')))),
                      expression(bold("75th percentile")),
                      list(expression(bold("50th percentile")),expression(bold("  (median)"))),
                      expression(bold("25th percentile")),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[1], "th percentile", sep=''))))),
                    values=c(4.2, 3.5, 1.5, 0, -1.2, -3.0)),
                  simple=list(z=list(stats=matrix(c(-3.0, -1.2 ,0, 1.5, 3.5),
                                       ncol=1),
                                n=54, names=""),
                    labels=list(expression(bold("Number of values")),
                      expression(bold("Maximum value")),
                      expression(bold("75th percentile")),
                      list(expression(bold("50th percentile")),expression(bold("  (median)"))),
                      expression(bold("25th percentile")),
                      expression(bold("Minimum value"))),
                    values=c(4.2, 3.5, 1.5, 0, -1.2, -3.0)),
                  extended=list(z=list(stats=matrix(c(-3., -1.5, 0, 1.5, 3.),
                                         ncol=1),
                                  n=54, names="",
                                  out=c(-3.5, 4.0), group=c(1,1)),
                    labels=list(expression(bold("Number of values")),
                      list(expression(bold("Individual value above the")),
                           as.expression(substitute(bold(x), list(x=paste("  ",
                               Box$truncated[2], "th percentile", sep=''))))),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[2], "th percentile", sep='')))),
                      expression(bold("75th percentile")),
                      expression(bold("50th percentile\n (median)")),
                      expression(bold("25th percentile")),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[1], "th percentile", sep='')))),
                      list(expression(bold("Individual value below the")),
                           as.expression(substitute(bold(x), list(x=paste("  ",
                               Box$truncated[1], "th percentile", sep='')))))),
                    values=c(4.5, 4.0, 3, 1.5, 0, -1.5, -3., -3.5)))
  bxp.stats <- function(x, range=1.5) {
    stats <- quantile(x, type=2, names=FALSE, na.rm=TRUE)
    if(range > 0) {
      iqr <- diff(stats[c(2, 4)])
      loout <- x < (stats[2] - iqr*range)
      stats[1] <- min(x[!loout])
      hiout <- x > (stats[4] + iqr*range)
      stats[5] <- max(x[!hiout])
      out <- x[loout | hiout]
    }
    else
      out <- numeric(0)
    stats <- as.matrix(stats)
    return(list(stats=stats, out=out, n=sum(!is.na(x))))
  }
  ## Create the rest of the explanation
  explan <- setExplan(list(name="boxplot", what="none", type="solid",
                           width="standard", symbol="circle", filled=TRUE,
                           size=0.09, color="black"))
  ## Remove NAs from data
  x <- lapply(x, function(xx) xx[!is.na(xx)])
  ## Process the data min and max needed for renderBoxPlot
  minx <- min(unlist(x), na.rm=TRUE)
  maxx <- max(unlist(x), na.rm=TRUE)
  ## Convert to common log if yaxis.log
  if(yaxis.log) {
    if(minx <= 0)
      stop("All data must be greater than 0 for log scale")
    x <- lapply(x, log10)
  }
  ## Range = 0 suppresses identification of outliers for simple and truncated
  if(type == "tukey")
    resbox <- lapply(x, bxp.stats)
  else
    resbox <- lapply(x, bxp.stats, range=0)
  ## Modify resbox to add needed info
  for(i in names(resbox)) {
    target <- resbox[[i]] # make it easy to modify
    target$farout <- numeric(0)
    target$censored <- target$estimated <- -Inf
    target$names <- i
    target$data <- x[[i]]
    if(type == "truncated" && target$n > Box$nobox) { # compute 10/90
      probs <- Box$truncated/100
      tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE)
      target$stats[1,1] <- tobind[1]
      target$stats[5,1] <- tobind[2]
    }
    else if(type == "extended" && target$n > Box$nobox) { # compute 10/90
      probs <- Box$truncated/100
      tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE)
      target$stats[1,1] <- tobind[1]
      target$stats[5,1] <- tobind[2]
      target$out <- x[[i]][x[[i]] < tobind[1] | x[[i]] > tobind[2]]
    }
    else if(type == "tukey" && target$n > Box$nobox) { # any far outs?
      farfence <- 3 * (target$stats[4,1] - target$stats[2,1])
      farout <- (target$out > target$stats[4,1] + farfence) |
        (target$out < target$stats[2,1] - farfence)
      if(any(farout)) { # put into the correct spot
        target$farout <- target$out[farout]
        target$out <- target$out[!farout]
      }
    }
    if(target$n <= Box$nobox) { # suppress box, plot points
      target$out <- target$data[!is.na(target$data)]
      target$stats[seq(5), 1] <- target$stats[3,1]
    }
    ## Replace the original
    resbox[[i]] <- target 
  }
  ## End of data processing
  resbox$data.range <- c(minx, maxx)
  z <- explain[[type]]
  z$fill <- Box$fill
  return(list(boxes=resbox, explan=explan, z=z))
}

## Method functions--basically assemble data and pass off to compute stats
boxPlot.list <- function(..., group=NULL, # data
                    Box=list(type="truncated", show.counts=TRUE, 
                      nobox=5, width="Auto", fill="none",
                      truncated=c(10,90)), # b&w controls
                    yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                    ylabels="Auto", xlabels="Auto", xlabels.rotate=FALSE, # labels
                    xtitle="", ytitle="",  caption="", # axis titles and caption
                    margin=c(NA,NA,NA,NA)) { # margin control
  ## Process data to plot, all must be class "list" and contain only numeric
  ##  data. This would be the classic behavior for 
  if(!is.null(group))
    stop("Lists cannot be grouped by boxPlot")
  dots <- c(...)
  xrange <- c(0, length(dots) + 1)
  xtoplot <- seq(length(dots))
  ## Fix defaults for Box
  Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
                     censorstyle="", nobox=5, width="Auto",  fill="none",
                     truncated=c(10,90))
  Box$type <- match.arg(Box$type, c("truncated", "simple",
  																	"tukey", "extended"))
  if(!is.na(Box$censorbox))
    warning(paste(Box$censorstyle, " boxplot not valid for these data, reset",
                  sep=''))
  ## Compute the stats and produce the boxplot
  if(dev.cur() == 1L)
    setGD("BoxPlot")
  statsret <- boxPlotStats(dots, Box, yaxis.log)
  ## What gets passed from statsret?
  retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
                          yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
                          xtitle, ytitle, caption, margin)
  invisible(retval)
}

boxPlot.data.frame <- function(..., group=NULL, # data
                               Box=list(type="truncated", show.counts=TRUE,
                                 nobox=5, width="Auto", fill="none",
                                 truncated=c(10,90)), # b&w controls
                               yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                               ylabels="Auto", xlabels="Auto",
                               xlabels.rotate=FALSE, # labels
                               xtitle="", ytitle="",  caption="", # axis titles and caption
                               margin=c(NA,NA,NA,NA)) { # margin control
  ## Process data to plot, all must be class "data.frame" and only numeric
  ##  data will be processed--all other classes of columns 
  if(!is.null(group))
    stop("Data frames cannot be grouped by boxPlot")
  dots <- c(...) # creates a list--drops data.frame attributes
  dots <- dots[sapply(dots, is.numeric)]
  xrange <- c(0, length(dots) + 1)
  xtoplot <- seq(length(dots))
  ## Fix defaults for Box
  Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
                     censorstyle="", nobox=5, width="Auto",  fill="none",
                     truncated=c(10,90))
  Box$type <- match.arg(Box$type, c("truncated", "simple",
  																	"tukey", "extended"))
  if(!is.na(Box$censorbox))
    warning(paste(Box$censorstyle, " boxplot not valid for these data, reset",
                  sep=''))
  ## Compute the stats and produce the boxplot
  if(dev.cur() == 1L)
    setGD("BoxPlot")
  statsret <- boxPlotStats(dots, Box, yaxis.log)
  ## What gets passed from statsret?
  retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
                          yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
                          xtitle, ytitle, caption, margin)
  invisible(retval)
}

boxPlot.numeric <- function(..., group=NULL, # data
                            Box=list(type="truncated", show.counts=TRUE,
                              nobox=5, width="Auto", fill="none",
                              truncated=c(10,90)), # b&w controls
                            yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                            ylabels="Auto", xlabels="Auto",
                            xlabels.rotate=FALSE, # labels
                            xtitle="", ytitle="",  caption="", # axis titles and caption
                            margin=c(NA,NA,NA,NA)) { # margin control
  ## Process data to plot, all must be numeric. If only 1, then can be grouped
  dots <- list(...)
  if(!is.null(group)) {
    if(length(dots) == 1) { # split the data, names derived from group
      if(!is.numeric(group))
        group <- recode(group, "", " ")
      dots <- split(dots[[1]], group)
      Ndots <- length(dots)
      if(is.numeric(group)) { # set xrange by data
        xrange <- range(group)
        if(Ndots > 1)
          meanspacing <- diff(xrange)/(Ndots-1)
        else
          meanspacing <- 1.0
        if(!is.numeric(xlabels))
          xlabels <- 7 # must force to numeric
        xrange <- xrange + c(-meanspacing, meanspacing)/2
        xtoplot <- as.double(names(dots))
      }
      else {
        xrange <- c(0, Ndots + 1)
        xtoplot <- seq(Ndots)
      } # end of xrange logic
    }
    else
      stop("Multiple numeric vectors cannot be grouped by boxPlot")
  }
  else {
    xrange <- c(0, length(dots) + 1)
    xtoplot <- seq(length(dots))
  }
  if(is.null(names(dots))) { # try to get names
    call <- as.list(match.call())[-1] # drop boxPlot
    call <- as.character(call)
    names(dots) <- call[seq(length(dots))]
  }
  ## Fix defaults for Box
  Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
                     censorstyle="", nobox=5, width="Auto", fill="none",
                     truncated=c(10,90))
  Box$type <- match.arg(Box$type, c("truncated", "simple",
  																	"tukey", "extended"))
  if(!is.na(Box$censorbox))
    warning(paste(Box$censorstyle, " boxplot not valid for these data, reset",
                  sep='')) # actually just ignored in call to this stats function
  ## Compute the stats and produce the boxplot
  if(dev.cur() == 1L)
    setGD("BoxPlot")
  statsret <- boxPlotStats(dots, Box, yaxis.log)
  ## What gets passed from statsret?
  retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
                          yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
                          xtitle, ytitle, caption, margin)
  invisible(retval)
}
