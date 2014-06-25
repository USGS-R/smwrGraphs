\name{contourPlot}
\alias{contourPlot}
\alias{preSurface}
\title{
Surface Plot
}
\description{
Produce a surface plot to show three-dimensional data or a colored surface plot to show 
four-dimensional data.
}
\usage{
preSurface(x, y, z.surf,
  zaxis.log = FALSE,  zaxis.range = c(NA, NA),
  yaxis.log = FALSE,  yaxis.range = c(NA, NA),
  xaxis.log = FALSE,  xaxis.range = c(NA, NA),
  xlabels = "Auto", ylabels = "Auto", zlabels = "Auto",
  phi = NA, theta = NA, batch = FALSE)
  
surfacePlot(pre, z.color="lightblue",
  Surface=list(name="", lineColor="black", levels=20, ramp="coolWarm"),
  xtitle="", ytitle="", ztitle="", margin = c(NA, NA, NA, NA), caption="")

\arguments{
  \item{x}{
the x-axis coordinates corresponding to rows for \code{z.surf}. Must be numeric or Dates
and in increasing order. Missing values are not permitted.
}
  \item{y}{
the y-axis coordinates corresponding to columns for \code{z.surf}. Must be numeric
and in increasing order. Missing values are not permitted.
}
  \item{z.surf}{
the surface to be plotted; must have \code{length(x)} rows and \code{length(y)} columns. 
Missing values are permitted, but result in blank areas on the surface.
}
  \item{Zaxis.log}{
logical: log transform z axis?
}
  \item{zaxis.range}{
set the range of the z-axis.
}
  \item{yaxis.log}{
logical: log transform y axis?
}
  \item{yaxis.range}{
set the range of the y-axis.
}
  \item{xaxis.log}{
logical: log transform x axis?
}
  \item{xaxis.range}{
set the range of the x-axis.
}
  \item{xlabels}{
set up x-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{zlabels}{
set up z-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{phi}{
the viewing angle relative to the x-y plane. If \code{NA}, then programmatically
select a reasonable angle. Should be greater than 0 and less than 90 degrees, but the best
angles are generally between 20 and 45 degrees.
}
  \item{theta}{
the viewing angle relative to the x-axis. Positive values rotate the x-y plane in a clock-wise 
direction. If \code{NA}, then programmatically select a reasonable angle by putting the largest
values of \code{z.surf} near the rear and the smallest values near the front. In general, the best angles are not multiples of 90 degrees.
}
  \item{batch}{
logical or character. If logical and \code{TRUE}, then select the viewing angles specified by
\code{phi} and \code{theta}. If logical and \code{FALSE}, then draw 9 candidate combinations of
viewing angles and pause for user input to select the desired viewing angles. If character, then
select the viewing angles specified by the letter selection, must be A through I.
}

  \item{pre}{
the output from \code{preSurface}.
}
  \item{z.color}{
the surface color. Can be either a character string indicating the color of the surface, or a
numeric matrix. If a matrix, then must have \code{length(x)} rows and \code{length(y)} columns
or \code{length(x) - 1} rows and \code{length(y)-1} columns. Missing values are permitted, but result in blank areas on the surface. See \bold{Details}.
}
  \item{Surface}{
control parameters for the surface. See \bold{Details}.
}

  \item{xtitle}{
the x-axis title.
}
  \item{ytitle}{
the y-axis title.
}
  \item{ztitle}{
the z-axis title.
}
  \item{margin}{
set up the plot area margins. To allocate space for a graph title, set the third value to
1.5, otherwise all values should be \code{NA} or 0.
}
\item{caption}{
the figure caption.
}
}
\details{
If \code{z.color} is a numeric matrix, then the values represent the average surface of the grid
defined by \code{x} and \code{y}, that is it represents the value one-half way between each
value of \code{x} and one-half way between each value of \code{y}, the mid point. If it has the
same dimensions as \code{z.surf}, then the data are resampled by averaging each of the four
corners to compute the mid point value. The values of \code{z.color} are assigned using the
controls in \code{Surface}.\cr

The \code{Surface} argument must be a tagged list with these components:
\describe{
	\item{name}{the name of \code{z.color} used in the explanation.}
	\item{lineColor}{the color for each line on the surface. If "none," then lines are not drawn.}
	\item{levels}{the levels of the surface colors. Maybe either a single numeric value that
	indicates the approximate number of levels, or a vector that indicates the exact breaks in the
	levels.}
	\item{ramp}{the name of the color ramp. May be "gray" or "grey" for a gray scale or the prefix
	name of a function that creates a range of colors, see \code{\link{coolWarm.colors}} for
	examples.}
}
}
\value{
Information about the graph.
}
\note{
A call must be made to \code{setPage} or \code{setPDF} to set up the graphics environment before calling \code{surfacePlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{persp}}
}
\keyword{hplot}
