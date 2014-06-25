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
control information to plot the observations.  See \code{\link{setPlot}} for a description
of the parameters.
}
  \item{varPlot}{
control information to plot the variables.  See \code{\link{setPlot}} for a description
of the parameters. For \code{yPlot}, symbol can be "arrow" to indicate that an arrow is to be
drawn from the origin.
}
  \item{obsPlotLabels}{
control information for the observation labels. See \bold{Details}.
}
  \item{varPlotLabels}{
control information for the variable labels. See \bold{Details}.
}
  \item{ylabels}{
set y-axis labels for the observation data.
}
  \item{xlabels}{
set x-axis labels for the observation data.
}
  \item{ylabels2}{
set y-axis labels for the variable data.
}
  \item{xlabels2}{
set x-axis labels for the variable data.
}
  \item{xtitle}{
x-axis title for the observation data.
}
  \item{ytitle}{
y-axis title for the observation data.
}
  \item{xtitle2}{
x-axis title for the variable data.
}
  \item{ytitle2}{
y-axis title for the variable data.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins.
}
  \item{\dots}{
not used, required for other methods.
}
}
\details{
The scaling between observations and variables is controlled by \code{Scale}, which can take any value between 0 and 1
or a character string indicating a specific scaling. The options for the character string are "distance," which produces
a plot where the observations retain their approximate relation with respect to Eucldiean distances and correponds to
a numeric value of 1; "variance," which produces a plot where the cosine of the angle between the variable vectors is related
to the correlation between the variables and correspondsd to a numeric value of 0; "symmetric," which tries to balance the range
of values for observations and variables to give a pleasing graph and corresponds to a numeric value of 0.5; or "Auto,"
which is the same as "variance." Another option for \code{Scale} is "interpolative," which produces a specialized axis scaling
so that the approximate values of the variables can be obtained for each observation. It is not implemented in this version.\cr

The \code{obsPlotLabels} and \code{varPlotLabels} arguments must be tagged lists with these components:
\describe{
	\item{labels}{the labels. For \code{xPlotLabels}, "rownames" means use the row names from
	\code{x} to generate the labels. For \code{yPlotLabels}, "colnames" means use the column names
	from \code{y} to generate the labels. Otherwise a character vector of the labels.}
  \item{dir}{the direction the label text is placed from the object.}
  \item{size}{the size of the label text.}
  \item{offset}{the distance the labels is placed relative to the object.}
  \item{color}{the color of the label text.}
}

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

