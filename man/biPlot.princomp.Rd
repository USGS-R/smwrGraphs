\name{biPlot.princomp}
\alias{biPlot.princomp}

\title{
Biplot
}
\description{
Produce a biplot, which is a plot of two different types of data on the same graph.
}
\usage{
\method{biPlot}{princomp}(x, Which = 1:2, Scale = "Auto", obsPlot = list(name = "observations", 
what = "points", type = "solid", width = "standard", symbol = "circle", filled = TRUE, size = 0.05, 
color = "black"), varPlot = list(name = "variables", width = "color", size = 0.2, color = "darkblue", 
symbol = "arrow", filled = FALSE), obsPlotLabels = list(labels = "rownames", dir = "NE", size = 8, 
offset = 0.75), varPlotLabels = list(labels = "colnames", dir = "Auto", size = 8, offset = 0.75, 
color = "darkblue"), ylabels = 5, xlabels = 5, ylabels2 = "Auto", xlabels2 = "Auto", xtitle = "Auto", 
ytitle = "Auto", xtitle2 = "Auto", ytitle2 = "Auto", caption = "", margin = c(NA, NA, NA, NA), ...)
}

\arguments{
  \item{x}{
an object of class "princomp" that has the information to to create a biplot.
}
  \item{Which}{
sequence of two numberis indicating which components to plot.
}
  \item{Scale}{
either a character string indicating the scaling option between observations and variables, 
or numeric value controling the scaling. If character, then must be one of "auto," "distance," "symmetric,"
"variance," or "interpolative." See \bold{Details}.
}
  \item{obsPlot}{
control information to plot the observations.
}
  \item{varPlot}{
control information to plot the variables.
}
  \item{obsPlotLabels}{
control information for the observation labels.
}
  \item{varPlotLabels}{
control information for the variable labels.
}
  \item{ylabels}{
set y-axis labels for \code{x} data.
}
  \item{xlabels}{
set x-axis labels for \code{x} data.
}
  \item{ylabels2}{
set y-axis labels for \code{y} data.
}
  \item{xlabels2}{
set x-axis labels for \code{y} data.
}
  \item{xtitle}{
x-axis title for \code{x} data.
}
  \item{ytitle}{
y-axis title for \code{x} data.
}
  \item{xtitle2}{
x-axis title for \code{y} data.
}
  \item{ytitle2}{
y-axis title for \code{y} data.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins
}
  \item{\dots}{
not used, required for method function.
}
}
\details{
The scaling between observations and variables is controlled by \code{Scale}, which can take any value between 0 and 1
or a character string indicating a specific scaling. The options for the character string are "distance," which produces
a plot where the observations retain their approximate relation with respect to Eucldiean distances and correponds to
a numeric value of 1; "variance," which produces a plot where the cosine of the angle between the variable vectors is related
to the correlation between the variables and correspondsd to a numeric value of 0; "symmetric," which tries to balance the range
of values for observations and variables to goive a pleasing graph and corresponds to a numeric value of 0.5; or "Auto,"
which is the same as "variance." Another option for \code{Scale} is "interpolative," which produces a specialized axis scaling
so that the approximate values of the variables can be obtained for each observation. It is not implemented in this version.
}
\value{
Information about the graph.
}
\note{
A call shoould be made to \code{setPage} to set up the graphics environment before calling \code{biPlot}.
}
\seealso{
\code{\link{setPage}}, \code{\link{biPlot}}
}
\keyword{ hplot }

