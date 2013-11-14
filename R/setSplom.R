# Set up a scatter plot matrix 
#
# Coding History:
#    2011Jul30 DLLorenz Original coding and begin tweaks
#    2012Oct27          This version.
#

setSplom <- function(size=NULL, # Size of splom, must be square
                      num.variables, show.all=FALSE,
                      touching=TRUE, # Parameters of the splom
                      explanation=NULL, # Where to put explanation
                      ymargin=3.5) { # Margins for y- (and x-)axis
  ## Arguments:
  ##  size (numeric scalar) the width and height of the graph area, exclusive
  ##    of explanation. If NULL, then use min of figure width and height.
  ##  num.variables (numeric scalar) the number of variables to plot
  ##  show.all (logical scalar) show all (TRUE) fill grid, or only lower tri
  ##    (FALSE)
  ##  touching (logical scalar) individual plots touch (TRUE) or have small
  ##   separation (FALSE).
  ##  explanation (tagged list or NULL) If NULL, then no EXPLANATION is needed,
  ##    or the EXPLANATION will  be placed in an unused rectangular array of
  ##    graphs (for show='lower').
  ##    If a list, then must be explanation=list(bottom=height), 
  ##    explanation=list(right=width).
  ##    The tag indicates where the explanation is placed relative to the array
  ##    of graphs. The value (height or width) indicates the height or width of
  ##    the explanation, the width of the graph area set up for the
  ##    explanation placed at the bottom is set by the width of the graph area
  ##    and vice versa.
  ##  ymargin (numeric scalar) and
  ##  xmargin (numeric scalar)indicate how much space to allocate on the left,
  ##    right, bottom, and top of the graph area for margins (space for labels 
  ##    yaxis titles). Space is always allocated at the bottom for a single 
  ##    line caption.
  ##
  if(is.null(size))
    size <- min(par('fin'))
  num.grid <- num.variables - (!show.all) # subtract 1 if lower, 0 if all
  shared <- as.double(!touching) # 0 for touching, 1 otherwise
  if(show.all) # allocate room at top/right for labels
    retval <- setLayout(width=size, height=size, num.cols=num.grid,
                        num.rows=num.grid, explanation=explanation,
                        shared.x=shared, shared.y=shared,yleft=ymargin,
                        yright=ymargin, xbottom=ymargin, xtop=ymargin)
  else
    retval <- setLayout(width=size, height=size, num.cols=num.grid,
                        num.rows=num.grid, explanation=explanation,
                        shared.x=shared, shared.y=shared,yleft=ymargin,
                        xbottom=ymargin)
  retval$show.all <- show.all
  retval$touching <- touching
  retval$num.variables <- num.variables
  invisible(retval)
}
