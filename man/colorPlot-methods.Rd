\name{colorPlot-methods}
\docType{methods}
\alias{colorPlot-methods}
\alias{colorPlot,numeric,numeric-method}
\title{Methods for Function \code{colorPlot}}
\description{
Produce a scatter plot where each point or group of related points has a unique color.
}
\usage{
\S4method{colorPlot}{numeric,numeric}(x, y, color, 
Plot = list(name = "Auto", what = "points", symbol = "circle", filled = TRUE, 
 size = 0.09, color = "Auto", groups = 4, ramp = "greenRed"), 
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), xaxis.log = FALSE, 
xaxis.range = c(NA, NA), ylabels = 7, xlabels = 7, 
xtitle = deparse(substitute(x)), ytitle = deparse(substitute(y)), caption = "", 
margin = c(NA, NA, NA, NA), ...)
}

\arguments{
  \item{x}{
the x-axis data
}
  \item{y}{
the y-axis data 
}
  \item{color}{
the colors or a class to set colors, must match the length of \code{x} and \code{y}.
}
  \item{Plot}{
control parameters of the plot
     name="Auto" means derive class names from the argument color,
      otherwise, must be a tagged list of color=name, ... (in which case
        the color tag is not used).
     color="Auto" means if color is double or dateLike create groups
       # of classes, otherwise create unique colors, alternate values
       are "Range" (treat like double), tagged list of group_name=color, and so forth,
       "Discrete" valid only for numeric, or
        "Index" valid only for integer or for specified colors.
}
  \item{yaxis.log}{
log-transform the y axis?
}
  \item{yaxis.rev}{
reverse the y axis?
}
  \item{yaxis.range}{
set the range of the y-axis.
}
  \item{xaxis.log}{
log-transform the x axis.
}
  \item{xaxis.range}{
set the range of the x-axis.
}
  \item{ylabels}{
set up y-axis labels.
}
  \item{xlabels}{
set up x-axis labels.
}
  \item{xtitle}{
the x-axis title.
}
  \item{ytitle}{
the y-axis title.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins.
}
  \item{\dots}{
additional argument for other methods.
}
}
\value{
Information about the graph.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{colorPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{xyPlot}}
}

\section{Methods}{
\describe{

\item{\code{signature(x = "numeric", y = "numeric")}}{
Create a colored scatter plot for numeric x- and y- coordinate data.
}
}}
\keyword{methods}
\keyword{hplot}
