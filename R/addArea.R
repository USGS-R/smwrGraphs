# add a polygon (area) to a graph
#
# Coding History:
#    2011Jun16 DLLorenz Original coding.
#    2011Oct24 DLLorenz Tweaks for package
#    2012Nov23 DLLorenz Added ybase option
#    2013Aug30 DLLorenz Bug Fix for border = "none"
#

addArea <- function(x, y, ybase=NULL, # data
                    Area=list(name="", color="gray",
                      outline="black"), # area controls
                    current=list(yaxis.log=FALSE, yaxis.rev=FALSE,
                      xaxis.log=FALSE)) { # current plot parameters
  ## Arguments:
  ##   x - the x-axis data
  ##   y - the y-axis data to plot
  ##   ybase - if not null shade between y and ybase
  ##   Area - parameters defining the characteristics of the area
  ##   current - the current plot information
  ##
  ## Process the data to plot
  y <- transData(y, current$yaxis.log, current$yaxis.rev,
                   current$ytrans, current$ytarg)
  x <- transData(as.double(x), current$xaxis.log, FALSE,
                 current$xtrans, current$xtarg)
  N <- length(x)
  if(!is.null(ybase)) { # Need to do something
    if(length(ybase) == 1L) { # a constant
      y <- c(ybase, y, ybase)
      x <- c(x[1L], x, x[N])
    }
    else { # ybase must be as long as y
      if(length(ybase) != N)
        stop("length of ybase must be one or match y")
      y <- c(y, rev(ybase))
      x <- c(x, rev(x))
    }
  }
  if(any(is.na(c(x, y))))
    stop("Missing value are not permitted in either x, y, or ybase")
  ## Set up plot information
  Area <- setDefaults(Area, name="", color="gray", outline="black")
  if(Area$outline == "none")
    Area$outline <- NA
  Plot <- setPlot(list(), name=Area$name, what="points", type="solid",
                  width="standard", symbol="none", filled=TRUE,
                  size=0.09, color="black", area.color=Area$color,
                  area.border=Area$outline) # force defaults if not set
  explan <- setExplan(Plot, old=current$explanation) # add info to set up explanation
  polygon(x, y, col=Area$color, border=Area$outline)
  current$x <- x
  current$y <- y
  current$explanation <- explan
  invisible(current)
}
