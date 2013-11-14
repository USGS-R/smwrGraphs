# Set up rectangular graph layout 
#
# Coding History:
#    2008Jun27 DLLorenz Original coding and begin tweaks
#    2010Nov30 DLLorenz Modified for R - use the layout() function
#    2011Oct24 DLLorenz Tweaks for package
#    2012Apr27 DLLorenz More grid debugging
#    2012Aug31 DLLorenz if top/right margins are set, use instead of NAs
#    2012Dec20 DLLorenz Bug fix for page eject
#    2013mar29 DLLorenz Tweak for explanation
#

setLayout <- function(width=NULL, height=NULL, # Size of graphs or graph area
                      num.cols=max(1, length(width)),
                      num.rows=max(1, length(height)),
                      num.graphs=num.rows*num.cols, # Graph grid
                      explanation=NULL, # Where to put explanation
                      shared.x=-1, shared.y=-1, # Share axes for multiple graphs
                      yleft=3.5, yright=0.5, # Margins for left and right axes
                      xbottom=3.2, xtop=0.5) { # Margin for bottom and top axis
  ## Arguments:
  ##  width (numeric scalar or vector) the width of the graph area, exclusive
  ##    of explanation, if a scalar or the width of the columns of individual
  ##    graphs if a vector. If NULL, then use defualt figure width.
  ##  height (numeric scalar or vector) the height of the graph area, exclusive
  ##    of explanation, if a scalar or the height of the rows of individual
  ##    graphs if a vector. If NULL, then use defualt figure height.
  ##  num.cols (numeric scalar) the number of columns in the rectangular
  ##    array of graphs. Computed from width if not supplied.
  ##  num.rows (numeric scalar) the number of rows in the rectangular
  ##    array of graphs. Computed from height if not supplied.
  ##  num.graphs (numeric scalar) the number of graphs in the rectangular
  ##    array of graphs. Computed from num.cols and num.rows if not supplied.
  ##    Note that if the EXPLANATION is to be placed in one of the array of graphs,
  ##    then num.graphs must be less than the product of num.cols and num.rows.
  ##  explanation (tagged list or NULL) If NULL, then no EXPLANATION is needed,
  ##    the EXPLANATION will be placed inside a graph, or the EXPLANATION will
  ##    be placed in an unused rectangular array of graphs (for example, 3
  ##    graphs in a 2x2 array with an explanation placed in the 4th graph location.
  ##    If a list, then must be explanation=list(bottom=height), 
  ##    explanation=list(right=width), or explanation=list(grid=number).
  ##    The tag indicates where the explanation is placed relative to the array
  ##    of graphs. The value (height or width) indicates the height or width of
  ##    the explanation, the width of the graph area set up for the
  ##    explanation placed at the bottom is set by the width of the graph area
  ##    and vice versa. If the tag is "grid," then the explanation will be set 
  ##    up in the specified grid cell
  ##  shared.x (numeric scalar) and
  ##  shared.y (numeric scalar) indicate how the axes are to be shared.
  ##    If < 0, then no sharing--each has own axis labels, etc. If = 0, then
  ##    axes in direct contact. If > 0, then the value indicates the relative
  ##    spacing.
  ##  yleft (numeric scalar) and
  ##  yright(numeric scalar) and
  ##  xbottom (numeric scalar) and
  ##  xtop (numeric scalar) indicate how much space to allocate on the left,
  ##    right, bottom, and top of the graph area for margins (space for labels 
  ##    yaxis titles). Space is always allocated at the bottom for a single 
  ##    line caption.
  ##
  ## Check to see if xtop or yright were set
  if(xtop != 0.5) # the default
    xtop.def <- xtop
  else
    xtop.def <- NA
  if(yright != 0.5) # the default
    yright.def <- yright
  else
    yright.def <- NA
  mar1 <- as.double(c(NA, NA, xtop.def, yright.def))
  ## Set up the page
  if(is.null(height)) {
    height <- par('fin')[2]
    if(!is.null(explanation$bottom))
      height <- height - explanation$bottom
  }
  if(is.null(width)) {
    width <- par('fin')[1]
    if(!is.null(explanation$right))
      width <- width - explanation$right
  }
  ## Instantiate the potentially computed arguments
  num.cols <- num.cols
  num.rows <- num.rows
  num.graphs <- num.graphs
  ## Easy--num.graphs=1
  retval <- list()
  if(num.graphs == 1) {
    if(is.null(explanation)) {
      mat <- matrix(1, nrow=1)
      widths <- width
      heights <- height
    }
    else if(!is.null(explanation$right)) {
      mat <- matrix(c(1, 2), nrow=1)
      widths <- c(width, explanation$right)
      heights <- height
      mar2 <- c(1.2,.5,0,0) # Leave room for caption and right
    }
    else if(!is.null(explanation$bottom)){ # Should be bottom
      mat <- matrix(c(1, 2), nrow=2)
      widths <- width
      heights <- c(height, explanation$bottom)
      mar2 <- c(1.2,0,.5,0)  # Leave room for caption and top
    }
    layout(mat, widths=lcm(widths*2.54), heights=lcm(heights*2.54))
    par(mar=c(0,0,0,0))
    plot.new()
    retval[[1]] <- list(margin=mar1, fig=par('fig'))
    if(!is.null(explanation)) { # put the explanation info in
      plot.new()
      retval$explanation <- list(margin=mar2, fig=par('fig'))
    }
    retval$size <- c(sum(widths), sum(heights))
    par(new=TRUE)
    return(retval)
  } # Done with a single graph and optional explanation
  ## Note that 1 margin line is par('csi')--set up lengths
  ## The 0.6 adjustment is not 100% clear, but the cex value changes after the
  ## first call to a plot (to 0.66). Go figure.
  ## If there ever comes a time when the boxes in a grid layout look funny,
  ## look here first!
  lineSize <- par('csi') * 0.6
  margins <- array(rep(mar1, each=num.rows*num.cols),
                   dim=c(num.rows, num.cols, 4))
  ## Set margins:
  ## For columns set the internal margins based on shared.y
  if(shared.y >= 0) {
    margins[, 1, 2] <- yleft
    margins[, 2:num.cols, 2] <- shared.y / 2
    margins[, num.cols, 4] <- yright
    margins[, 1:(num.cols - 1), 4] <- shared.y / 2
    if(shared.x < 0) { # Need to populate margins so that graphs align
      margins[1:num.rows, , 3] <- xtop
      margins[1:num.rows, , 1] <- xbottom
    }
  } # Otherwise leave NA
  ## For rows set the internal margins based on shared.x
  if(shared.x >= 0) {
    margins[1, , 3] <- xtop
    margins[2:num.rows, , 3] <- shared.x / 2
    margins[num.rows, , 1] <- xbottom
    margins[1:(num.rows - 1), , 1] <- shared.x / 2
    if(shared.y < 0) { # Need to populate margins so the graphs align
      margins[, 1:num.cols, 2] <- yleft
      margins[, 1:num.cols, 4] <- yright
    }
  } # otherwise leave NA
  ## Automagically allocate width and height of grid cells, if requested.
  ## Height of rows first
  if(length(height) == 1 && num.rows > 1) {
    if(shared.x < 0) {
      topspace <- rep(xtop, num.rows)
      botspace <- rep(xbottom, num.rows)
      if(is.null(explanation$bottom)) # Need to provide for caption
        botspace[num.rows] <- xbottom + 1
    }
    else { # Works for all non negative values of shared.x
      topspace <- c(xtop, rep(shared.x / 2, num.rows - 1))
      botspace <- c(rep(shared.x / 2, num.rows - 1), xbottom)
      if(is.null(explanation$bottom)) # Need to provide for caption
        botspace[num.rows] <- xbottom + 1
    }
    dist.h <- (height - sum(topspace + botspace) * lineSize) / num.rows
    height <- dist.h + (topspace + botspace) * lineSize
  }
  ## Width of columns second
  if(length(width) == 1 && num.cols > 1) {
    if(shared.y < 0) {
      leftspace <- rep(yleft, num.cols)
      rghtspace <- rep(yright, num.cols)
    }
    else { # Works for all non negative values of shared.y
      leftspace <- c(yleft, rep(shared.y / 2, num.cols - 1))
      rghtspace <- c(rep(shared.y / 2, num.cols - 1), yright)
    }
    dist.w <- (width - sum(leftspace + rghtspace) * lineSize) / num.cols
    width <- dist.w + (leftspace + rghtspace) * lineSize
  }
  ## Set up layout initial matrix (exclude right or bottom explanation)
  Seq <- seq(1, num.cols*num.rows)
  if(!is.null(explanation) && !is.null(explanation$grid)) {
    Egrid <- explanation$grid
    ## Check to see that num.graphs < num.rows * num.cols
    if(num.graphs >= num.rows * num.cols)
      stop('No room for explanation in grid')
    Seq[Seq > Egrid] <- Seq[Seq > Egrid] - 1
    Seq[Egrid] <- num.graphs + 1
    Seq[Seq > (num.graphs + 1)] <- 0
  }
  else # Explanation somewhere else
    Seq[Seq > num.graphs] <- 0
  mat <- matrix(Seq, num.rows, byrow=TRUE)
  if(is.null(explanation) || !is.null(explanation$grid)) { # No augmentation
    widths <- width
    heights <- height
  }
  else if(!is.null(wid <- explanation$right)) { # Put on right
    widths <- c(width, wid)
    heights <- height
    mat <- cbind(mat, num.graphs+1)
  }
  else if(!is.null(bot <- explanation$bottom)) { # Put on bottom
    widths <- width
    heights <- c(height, bot)
    mat <- rbind(mat, num.graphs+1)
  }
  else
    stop('Invalid explanation')
  ## Check for error in plot size--too small for text size
  if(min(c(widths, heights)) < 6.5 * lineSize)
    stop('Too many graphs for page/figure size')
  layout(mat, widths=lcm(widths*2.54), heights=lcm(heights*2.54))
  ## Populate the returned index of margins and graphs areas
  graphcount <- 0
  par(mar=c(0,0,0,0))
  for(i in seq(num.rows))
    for(j in seq(num.cols))
      if(mat[i,j] <= num.graphs && mat[i,j] > 0) {
        plot.new()
        retval[[mat[i,j]]] <- list(margin=margins[i,j,], fig=par('fig'))
        graphcount <- graphcount + 1
      }
  ## Reset cex so that the margins are actually set correctly!
  ## This si the change of Apr27
  par(cex=1)
  ## Now figure out what to do with the explanation
  if(is.null(explanation))
    ## If null, then copy last grid into the explanation tag
    ##  (avoids lazy explanation)
    retval$explanation <- retval[[graphcount]]
  else { # Put the explanation in the correct place
    if(!is.null(explanation$grid))
      ExplanGr <- explanation$grid
    else # Must be either bottom or right
      ExplanGr <- num.graphs + 1
    for(i in seq(graphcount + 1, ExplanGr)) # Reduce loops from 3:4 to 4:4
      plot.new()
    ## Set the explanation
    retval$explanation <- list(margin=rep(0,4), fig=par('fig'))
  }
  ## Finally the size of the total graph area
  retval$size <- c(sum(widths), sum(heights))
  par(new=TRUE)
  return(retval)
}
