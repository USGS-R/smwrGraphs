\name{transPlot}
\alias{transPlot}

\title{
X-Y Plot
}
\description{
Produce an x-y plot using arbitrary monotonic transforms for the axes.
}
\usage{
transPlot(x, xtrans, xinv, xtargs = NULL, y, ytrans, yinv, ytargs = NULL, 
Plot = list(name = "", what = "points", type = "solid", width = "standard", 
symbol = "circle", filled = TRUE, size = 0.09, color = "black"), yaxis.range = c(NA, NA), 
xaxis.range = c(NA, NA), ylabels = "Auto", xlabels = "Auto", xtitle = deparse(substitute(x)), 
ytitle = deparse(substitute(y)), caption = "", margin = c(NA, NA, NA, NA))
}

\arguments{
  \item{x}{
the x-axis data.
}
  \item{xtrans}{
the transformation function for the x-axis.
}
  \item{xinv}{
the inverse transformation for the x-axis.
}
  \item{xtargs}{
additional arguments to \code{xtrans} and \code{xinv}, as a list if necessary, NULL otherwise.
}
  \item{y}{
the y-axis data.
}
  \item{ytrans}{
the transformation function for the y-axis.
}
  \item{yinv}{
the inverse transformation for the y-axis.
}
  \item{ytargs}{
additional arguments to \code{ytrans} and \code{yinv}, as a list if necessary, NULL otherwise.

}
  \item{Plot}{
control parameters of the plot.
}
  \item{yaxis.range}{
set the range of the y-axis.

}
  \item{xaxis.range}{
set the range of the x-axis.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{transPretty}} for details.
}
  \item{xlabels}{
set up x-axis labels. See \code{\link{transPretty}} for details.
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
}
\value{
Information about the graph.
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{transPlot}.
}
\seealso{
\code{\link{setPage}}, \code{link{transPretty}}
}
\keyword{ hplot }
