#' Dot Plot
#' 
#' Creates a dot plot.
#' 
#' The \code{what} component of the \code{Plot} argument must be either
#' "points" or "none."
#' 
#' For linear axes, the range can be set to virtually any pair of values. For 
#'log axes, the choice of range is more resticted---for less than one log-cycle,
#'powers of whole numbers can be used; from 1 to about 3 log cycles, the choces 
#'should be powers of 3 or 10; and for more than 3 log cycles, the range sould be 
#'expressed only in powers of 10.
#'
#' @name dotPlot
#' @rdname dotPlot
#' @aliases dotPlot dotPlot,numeric-method dotPlot,Date-method
#' @param x the x-axis data. Missing values are permitted and not plotted.
#' @param y the y-axis data, expected to be be either character or factor.
#'Missing values are permitted and removed before plotting.
#' @param Plot control parameters of the plot, see \code{link{setMultiPlot}}
#' and \bold{Details} for details.
#' @param yaxis.orient orientation of the y-axis values, must be either "table"
#' or "grid." "Table" is sorted from top to bottom, "grid" is sorted from
#' bottom to top.
#' @param yaxis.order the order of the y-axis values, must be one of "none,"
#' "ascending," or "descending."
#' @param yaxis.grid logical, if \code{TRUE}, then draw grid lines.
#' @param xaxis.log logical, if \code{TRUE}, then log-transform the x axis.
#' @param xaxis.range set the range of the x-axis. See \bold{Details}.
#' @param ylabels set up y-axis labels.
#' @param xlabels set up x-axis labels.
#' @param xtitle x-axis title (also called x-axis caption).
#' @param ytitle y-axis title (also called y-axis caption).
#' @param caption the figure caption.
#' @param margin set the plot area margins, in units of lines of text. Generally
#'all NA or the output from \code{setGraph} if appropriate.
#' @param jitter.y logical, if \code{TRUE}, then adjust \code{y} values to reduce
#'overlap for each group, or adjust randomly if no groups. If \code{FALSE}, then
#'no adjustment is made. 
#' @param ... arguments for specific methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{dotPlot}.
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{signature(x = "numeric"))}{ Create a dot plot for numeric
#' x-coordinate data and any (discrete) y-coordinate data. } 
#' \item{signature(x = "Date"))}{ Create a dot plot for Date
#' x-coordinate data and any (discrete) y-coordinate data. } }
#' @seealso \code{\link{setPage}}, \code{\link{boxPlot}}
#' @keywords methods hplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- runif(12)
#' Y <- LETTERS[1:12]
#' setGD()
#' dotPlot(X, Y)
#' # For more details of dotPlot see
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @exportMethod dotPlot
setGeneric("dotPlot", function(x, y, Plot=list(),
                               yaxis.orient="", yaxis.order="", yaxis.grid=TRUE,
                               xaxis.log=FALSE, xaxis.range=c(NA,NA),
                               ylabels="", xlabels=7, xtitle="", ytitle="",
                               caption="", margin=c(NA, NA, NA, NA), ...)
           standardGeneric("dotPlot")
					 # Coding History:
					 #    2011Jun23 DLLorenz Original coding.
					 #    2011Aug03 DLLorenz Added axis labeling info to current
					 #    2011Oct24 DLLorenz Tweaks for package
					 #    2012Aug28 DLLorenz dots for future methods
					 #    2012Sep27 DLLorenz Made generic
					 #    2013Jan04 DLLorenz Added multiPlot option
					 #    2013Apr09 DLLorenz Added setGD
					 #    2013Aug19 DLLorenz Added Date method
					 #    2014Jun26 DLLorenz Converted to roxygen
					 )

#' @rdname dotPlot
setMethod("dotPlot", signature("numeric"), # "ANY" ignored in last position
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.orient="table", yaxis.order="none",
         yaxis.grid=TRUE, # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels="full",
         xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle="", # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), jitter.y=TRUE, ...) { # margin controls
  ##
  ## create the plotting positions
  ## set up the axes
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  yaxis.orient <- match.arg(yaxis.orient, c("table", "grid"))
  if(length(yaxis.order) == 1)
    yaxis.order <- match.arg(yaxis.order, c("none", "ascending", "descending"))
  ylabels <- match.arg(ylabels, c("full", "abbreviate"))
  if(dev.cur() == 1)
    setGD("DotPlot")
  ## Quick fix for numeric Y
  if(is.numeric(y))
    y <- as.character(y)
  ## Remove missing ys
  good <- !is.na(y)
  x <- x[good]
  y <- y[good]
  yax <- namePretty(y, orientation=yaxis.orient, order=yaxis.order,
                    label.abbr=ylabels == "abbreviate")
  ylev <- yax$labels
  y <- numericData(y, ylev)
  if(is.list(xlabels))
    xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=xlabels)
  
  xax <- do.call("setAxis", xax)
  x <- xax$data
  xax <- xax$dax
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up plot
  plot(x, y, type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  if(yaxis.grid) # draw light, gray lines
    segments(x0=xax$range[1], y0=y, x1=xax$range[2], y1=y,
             col="gray", lty=1, lwd=frameWt())
  ## Process plot control
  what=Plot$what[1]
  parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black")
  plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE) # part of returned data
  ## Draw each point
  if(jitter.y) {
    Grps <- unique(plot.info$name)
    if(length(Grps) == 1L)
      jitter.y <- runif(length(y), -.3333, .3333)
    else {
      Rng <- .4 - exp(-length(Grps)) # more-or-less works to expand range
      jitter.y <- seq(-Rng, Rng, length.out=length(Grps))
      names(jitter.y) <- Grps
      jitter.y <- jitter.y[plot.info$name] # one for each!
    }
  }
  points(x, y + jitter.y, type="p", pch=plot.info$pch, cex=plot.info$cex,
         col=plot.info$col, bg=ifelse(plot.info$filled, plot.info$col, 0))
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, yaxis.log=FALSE, yaxis.rev=FALSE,
                  yaxis.lev=ylev,
                  xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
                  yax=yax, xax=xax)))
}
)

#' @rdname dotPlot
setMethod("dotPlot", signature("Date"), # "ANY" ignored in last position
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.orient="table", yaxis.order="none",
         yaxis.grid=TRUE, # y-axis controls
         xaxis.log=FALSE, xaxis.range=range(x, na.rm=TRUE), # x-axis controls
         ylabels="full",
         xlabels="Auto", # labels
         xtitle="",
         ytitle="", # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), jitter.y=TRUE, ...) { # margin controls
  ## create the plotting positions
  ## set up the axes
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  yaxis.orient <- match.arg(yaxis.orient, c("table", "grid"))
  if(length(yaxis.order) == 1)
    yaxis.order <- match.arg(yaxis.order, c("none", "ascending", "descending"))
  ylabels <- match.arg(ylabels, c("full", "abbreviate"))
  if(dev.cur() == 1)
    setGD("DotPlot")
  ## Quick fix for numeric Y
  if(is.numeric(y))
    y <- as.character(y)
  ## Remove missing ys
  good <- !is.na(y)
  x <- x[good]
  y <- y[good]
  yax <- namePretty(y, orientation=yaxis.orient, order=yaxis.order,
                    label.abbr=ylabels == "abbreviate")
  ylev <- yax$labels
  y <- numericData(y, ylev)
  xaxis.log=FALSE # Force the issue
  dax <- datePretty(xaxis.range, major=xlabels)
  x <- numericData(x)
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up plot
  plot(x, y, type="n", xlim=dax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  if(yaxis.grid) # draw light, gray lines
    segments(x0=dax$range[1], y0=y, x1=dax$range[2], y1=y,
             col="gray", lty=1, lwd=frameWt())
  ## Process plot control
  what=Plot$what[1]
  parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black")
  plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE) # part of returned data
  ## Draw each point
  if(jitter.y) {
    Grps <- unique(plot.info$name)
    if(length(Grps) == 1L)
      jitter.y <- runif(length(y), -.3333, .3333)
    else {
      Rng <- .4 - exp(-length(Grps)) # more-or-less works to expand range
      jitter.y <- seq(-Rng, Rng, length.out=length(Grps))
      names(jitter.y) <- Grps
      jitter.y <- jitter.y[plot.info$name] # one for each!
    }
  }
  points(x, y + jitter.y, type="p", pch=plot.info$pch, cex=plot.info$cex,
         col=plot.info$col, bg=ifelse(plot.info$filled, plot.info$col, 0))
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, yaxis.log=FALSE, yaxis.rev=FALSE,
                  yaxis.lev=ylev,
                  xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
                  yax=yax, xax=dax)))
}
)
