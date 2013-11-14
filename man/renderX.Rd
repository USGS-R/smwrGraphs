\name{renderX}
\alias{renderX}
\alias{renderY}
\alias{ticks.render}
\title{
Label Axes 
}
\description{
Adds ticks, labels, or grids to an axis (support functions).
}
\usage{
renderX(pretty, bottom = list(ticks = TRUE, labels = TRUE, grid = FALSE, finegrid = FALSE, 
angle = 0), top = list(ticks = TRUE, labels = FALSE, grid = FALSE, finegrid = FALSE, angle = 0), 
bottitle = "X-AXIS TITLE", toptitle = "", caption = "")
renderY(pretty, left = list(ticks = TRUE, labels = TRUE, grid = FALSE, finegrid = FALSE), 
right = list(ticks = TRUE, labels = FALSE, grid = FALSE, finegrid = FALSE), 
lefttitle = "Y-AXIS TITLE", righttitle = "")
ticks.render(arg1, side, lwd)
}

\arguments{
  \item{pretty}{
the output from one of the "pretty" functions.
}
  \item{bottom}{
control parameters for the bottom x axis.
}
  \item{top}{
control parameters for the top x axis.
}
  \item{bottitle}{
the title for the bottom x axis.
}
  \item{toptitle}{
the title for the top x axis.
}
  \item{caption}{
the figure caption.
}
  \item{left}{
control parameters for the left y axis.
}
  \item{right}{
control parameters for the right y axis.
}
  \item{lefttitle}{
the title for the left y axis.
}
  \item{righttitle}{
the title for the right y axis.
}
  \item{arg1}{
control parameters for the tick locations.
}
  \item{side}{
the number of the axis, 1 is bottom, 2 is left, and so forth.
}
  \item{lwd}{
the line weight for the ticks.
}
}
\value{
Nothing is returned.
}
\keyword{ aplot }
