# interpolate points from a line 
#
# Coding History:
#    2012Sep14 DLLorenz Original coding.
#    2012Sep17          This version.
#

interpLine <- function(object, # output from call to plotXXX
                       xfromy, yfromx, warn=TRUE, ...) { # convert from to
  ## Arguments:
  ##   object - the output from any call to plot that draw a line
  ##   xfromy - the y-coordinate data to interpolate to x
  ##   yfromx - the x-coordinate data to interpolate to y
  ##   ... - any additional arguments
  ##
  ## Notes:
  ##  For any plot function that does not transform the x- or y-axis
  ##  data, like probPlot, an argument x-axis.trans or yaxis.trans must
  ##  be included to indicate suppression of the transform.
  ##
  if(!missing(xfromy) && missing(yfromx)) {
    y <- numericData(xfromy, lev=object$yaxis.lev)
    if(is.null(object$yaxis.trans))
      y <- transData(y, object$yaxis.log, object$yaxis.rev,
                     object$ytrans, object$ytarg)
    else
      y <- transData(y, FALSE, object$yaxis.rev)
    retval <- approx(object$y, object$x, xout=y)$y
    if(is.na(object$xaxis.log)) {
      ## Warning only needed if transformation is needed
      if(warn && !is.null(object$xaxis.trans))
        warning("Cannot back-transform x data from arbitrary transform,\n",
                "user responsible for completing back-transformation")
    }
    else if(object$xaxis.log)
      retval <- 10^retval
    return(retval)
  }
  if(!missing(yfromx) && missing(xfromy)) {
    x <- numericData(yfromx, lev=object$xaxis.lev) # Convert to consistent numeric
    if(is.null(object$xaxis.trans))
      x <- transData(x, object$xaxis.log, FALSE,
                     object$xtrans, object$xtarg)
    else # Supress transform
      x <- transData(x, FALSE, FALSE)
    retval <- approx(object$x, object$y, xout=x)$y
    if(is.na(object$yaxis.log)) {
      ## Warning only needed if transformation is needed
      if(warn && !is.null(object$yaxis.trans))
        warning("Cannot back-transform y data from arbitrary transform,\n",
                "user responsible for completing back-transformation")
    }
    else if(object$yaxis.log)
      retval <- 10^retval
    return(retval)
  }
  stop("Exactly one of xfromy or yfromx must be specified in the call")
}
