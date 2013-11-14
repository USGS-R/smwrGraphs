# Adds plots to a Piper diagram.
#    Calls ternarySubplot and piperSubplot..
#
# Coding history:
#    2009Oct28 DLLorenz Original coding and revisions.
#    2011May24 DLLorenz Conversion to R
#    2011Oct24 DLLorenz Tweaks for package
#    2011Oct24          This version.
#

addPiper <- function(xCat, yCat, zCat, xAn, yAn, zAn, xPip, yPip,
                     ## data (need not sum to 100); xP, yP are the piper x, y
                     Plot=list(name="", what='points', type='solid',
                       width='standard', symbol='circle', filled=TRUE,
                       size=0.09, color='black'), # plot controls (for all points)
                     current=list()) { # current Piper diagram parameters
  ## Arguments:
  ##  xCat (numeric vector) and
  ##  yCat (numeric vector) and
  ##  zCat (numeric vector) the cations for the x-, y-, and z-axes. Need not sum to
  ##    1 or 100.
  ##  xAn (numeric vector) and
  ##  yAn (numeric vector) and
  ##  zAn (numeric vector) the anions for the x-, y-, and z-axes. Need not sum to
  ##    1 or 100.
  ##  xPip (numeric vector) and
  ##  yPip (numeric vector) the coordinates for the internal piper diagram.
  ##  Plot (list) defining the characteristics of the plot
  ##  current (list) the special axis parameters for the current plot.
  ## Note: The plotting arguments must be either xCat, yCat, zCat, xAn, yAn, and 
  ## zAn (in which case they do not need to sum to 1 or 100), or xCat, yCat,
  ## xAn, yAn, xPip and yPip (in which case the values are the actual coordinates
  ## to be plotted).
  ## 
  ## Process plot controls
  Plot <- setPlot(Plot, name="", what='points', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black')
  explan <- setExplan(Plot, current$explanation)
  ## normalize the data according to range
  ## if zCat and zAn are not missing
  if(!missing(zCat) && !missing(zAn)) {
    axis.range <- current$axis.range
    tsum <- sumComposition(xCat, yCat, zCat, Range=axis.range[2])
    cations <- as.data.frame(tsum)
    names(cations) <- c(deparse(substitute(xCat)), deparse(substitute(yCat)),
                       deparse(substitute(zCat)))
    ca <- tsum[,1]
    mg <- tsum[,2]
    na.k <- tsum[,3]
    tsum <- sumComposition(xAn, yAn, zAn, Range=axis.range[2])
    anions <- as.data.frame(tsum)
    names(anions) <- c(deparse(substitute(xAn)), deparse(substitute(yAn)),
                        deparse(substitute(zAn)))
    cl.f.no2.no3 <- tsum[,1]
    co3.hco3 <- tsum[,2]
    so4 <- tsum[,3]
    ## get the x-y data for the ternary plots
    cations <- cbind(cations, ternarySubplot(ca, mg, na.k,
                                             axis.range=axis.range, plot=FALSE))
    xCat <- cations$x
    yCat <- cations$y
    anions <- cbind(anions, ternarySubplot(cl.f.no2.no3, co3.hco3, so4,
                             axis.range=axis.range, orient="a", plot=FALSE))
    xAn <- anions$x
    yAn <- anions$y
  ## data for the piper diagram
    yPip <- so4 + cl.f.no2.no3
    piper <- as.data.frame(piperSubplot(na.k, yPip, axis.range=axis.range,
                                        plot=FALSE))
    yPip <- piper$y
    xPip <- piper$x
    
  } # end of data processing for raw data
  else { # package the already processed data.
    cations <- data.frame(x=xCat, y=yCat)
    anions <- data.frame(x=xAn, y=yAn)
    piper <- data.frame(x=xPip, y=yPip)
  }
  ## Plot it
  oldpars <- par(no.readonly=TRUE)
  ## Set up subregion for cation plot
  par(current$catplot)
  par(fin=oldpars$fin) # this must be set separately! includes all calls below
  points(xCat, yCat, type=explan$current$type,
         lwd=explan$current$lwd, lty=explan$current$lty,
         pch=explan$current$pch, cex=explan$current$cex,
         col=explan$current$col, bg=explan$current$col)
  par(current$anplot)
  par(fin=oldpars$fin)
  points(xAn, yAn, type=explan$current$type,
         lwd=explan$current$lwd, lty=explan$current$lty,
         pch=explan$current$pch, cex=explan$current$cex,
         col=explan$current$col, bg=explan$current$col)
  par(current$piperplot)
  par(fin=oldpars$fin)
  points(xPip, yPip, type=explan$current$type,
         lwd=explan$current$lwd, lty=explan$current$lty,
         pch=explan$current$pch, cex=explan$current$cex,
         col=explan$current$col, bg=explan$current$col)
  par(oldpars)
  par(fin=oldpars$fin)
  ## return
  ##Note that plot info does not pertain here
  retval <- list(cations=cations, anions=anions, piper=piper,
                 catplot=current$catplot, anplot=current$anplot,
                 piperplot=current$piperplot, explanation=explan)
  invisible(retval)
}
