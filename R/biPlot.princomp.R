# Create a biplot from a principal components analysis
# This code creates biplots as decribed in Joliffe that correspond to those
#  descriptions of how scale works
#
# Coding History:
#    2010Mar18 DLLorenz Original coding, from the revised biplot.princomp code
#    2011Apr15 DLLorenz Begin modifications for R
#    2011Oct24          This version.
#

biPlot.princomp <- function(x, Which=1:2, # data
                            Scale='Auto',
                            obsPlot=list(name="observations", what='points', 
                              type='solid', width='standard', symbol='circle', 
                              filled=TRUE, size=0.05, color='black'),
                            varPlot=list(name='variables', width='color',
                              size=0.2, color='darkblue', symbol='arrow',
                              filled=FALSE), # plot controls
                            obsPlotLabels=list(labels='rownames', dir='NE',
                              size=8, offset=0.75),
                            varPlotLabels=list(labels='colnames', dir='Auto',
                              size=8, offset=0.75, color='darkblue'), # object labels
                            ylabels=5,  xlabels=5,
                            ylabels2='Auto',  xlabels2='Auto', # axis labels
                            xtitle='Auto', ytitle='Auto', 
                            xtitle2='Auto', ytitle2='Auto', # axis titles
                            caption="", # caption
                            margin=c(NA, NA, NA, NA), ...) {
  ## Arguments
  ##  x is  a princomp object that has the information to to create a biplot
  ##  ... are unused required arguments for method function
  if(length(Which) != 2)
    stop("length of choices must be 2")
  scores <- x$scores
  if(!length(scores))
    stop("x must contain scores")
  loadings <- x$loadings
  if(!length(loadings))
    stop("x must contain loadings")
  ## may add options as new classes are created
  lambdas <- x$sdev
  ## prepare the data
  obs <- scores[, Which]
  vars <- loadings[, Which]
  lambdas <- lambdas[Which]
  p <- dim(vars)[1]
  n <- dim(obs)[1]
  if(is.character(Scale)) {
    Scale <- match.arg(tolower(Scale), c("auto", "distance", "symmetric",
                                         "variance", "interpolative"))
    if(Scale == "auto")
      Scale <- "variance"
    ## set titles if necessary
    if(all(x$scale == 1))
      scaled <- "" # raw data
    else
      scaled <- "scaled " # these have been scaled to mean 0 and sd 1.
    if(xtitle == 'Auto')
      xtitle <- switch(Scale ,
                       distance=paste("Component ", Which[1], " (", scaled,
                         "Euclidean distance)", sep=''),
                       variance=paste("Component ", Which[1], " (", scaled,
                         "Mahalanobis distance)", sep=''),
                       symmetric=paste("Component ", Which[1], sep=''),
                       interpolative=paste("Component ", Which[1], " (", scaled,
                         "Euclidean distance)", sep=''))
    if(ytitle == 'Auto')
      ytitle <- switch(Scale ,
                       distance=paste("Component ", Which[2], " (", scaled,
                         "Euclidean distance)", sep=''),
                       variance=paste("Component ", Which[2], " (", scaled,
                         "Mahalanobis distance)", sep=''),
                       symmetric=paste("Component ", Which[2], sep=''),
                       interpolative=paste("Component ", Which[2], " (", scaled,
                         "Euclidean distance)", sep=''))
    if(xtitle2 == 'Auto')
      xtitle2 <- switch(Scale ,
                        distance=paste(scaled, "variable contribution", sep=''),
                        variance=paste(scaled, "variable standard deviation", sep=''),
                        symmetric="",
                        interpolative="")
    if(ytitle2 == 'Auto')
      ytitle2 <- switch(Scale ,
                        distance=paste(scaled, "variable contribution", sep=''),
                        variance=paste(scaled, "variable standard deviation", sep=''),
                        symmetric="",
                        interpolative="")
    ## set margins 3 and 4 (to default value that allow reasonable room)
    margin[4] <- margin[3] <- switch(Scale,
                                     distance=-4.1, variance=-4.1,
                                     symmetric=0.5, interpolative=0.5)
    ## Set scale to numeric value for correct computation of obs and var
    scale <- switch(Scale,
                    distance=1, variance=0,
                    symmetric=0.5, interpolative=-1)
  }
  else {
    scale <- Scale # better be numeric
    ## Now set the titles and top and right margins
    if(xtitle == 'Auto')
      xtitle <- paste("Component ", Which[1], sep='')
    if(ytitle == 'Auto')
      ytitle <- paste("Component ", Which[2], sep='')
    if(xtitle2 == 'Auto')
      xtitle2 <- ""
    if(ytitle2 == 'Auto')
      ytitle2 <- ""
    margin[4] <- margin[3] <- 0.5
  }
  if(scale < 0) { # this produces an interpolation biplot
    vars <- vars # this needs work, see Joliffe
    warning("Interpolation not yet implemented")
  }
  else if(scale >= 0 && scale <= 1) {
    vars <- vars * rep(lambdas^(1 - scale), c(p, p))
    obs <- obs * rep(lambdas^(scale - 1), c(n, n))
  }
  else stop("bad value for scale")
  ## Call biplot.default to do the plotting!
  invisible(biPlot.default(obs, vars, is.character(Scale),
                           xPlot=obsPlot, yPlot=varPlot,
                           xPlotLabels=obsPlotLabels,
                           yPlotLabels=varPlotLabels,
                           ylabels=ylabels, xlabels=xlabels,
                           ylabels2=ylabels2, xlabels2=xlabels2,
                           xtitle=xtitle, ytitle=ytitle,
                           xtitle2=xtitle2, ytitle2=ytitle2,
                           caption=caption, margin=margin))
}
