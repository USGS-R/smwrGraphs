\name{seasonPlot-methods}
\docType{methods}
\alias{seasonPlot-methods}
\alias{seasonPlot,ANY,numeric-method}
\alias{seasonPlot,character,numeric-method}
\title{Methods for Function \code{seasonPlot}}
\description{
Create a plot of data on a yearly cycle.
}
\usage{
\S4method{seasonPlot}{ANY,numeric}(x, y,
Plot=list(name = "", what = 'points', type = 'solid', width = 'standard', symbol = 'circle',
 filled = TRUE, size = 0.09, color = 'black'),
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = c("calendar", "water", "climate"),
ylabels = 7, xlabels = "Auto",
xtitle = "", ytitle = deparse(substitute(y)), caption = '', 
margin = c(NA, NA, NA, NA), ...)

\S4method{seasonPlot}{character,numeric}(x, y,
Plot=list(name = "", what = 'lines', type = 'solid', width = 'standard', symbol = 'circle',
 filled = TRUE, size = 0.09, color = 'black'),
yaxis.log = FALSE, yaxis.rev = FALSE, yaxis.range = c(NA, NA), 
xaxis.range = c("calendar", "water", "climate"),
ylabels = 7, xlabels = "Auto",
xtitle = "", ytitle = deparse(substitute(y)), caption = '', 
margin = c(NA, NA, NA, NA), ...)
}

\arguments{
  \item{x}{
the x-coordinate data.
}
  \item{y}{
the y-coordinate data.
}
  \item{Plot}{
control parameters of the plot
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
  \item{xaxis.range}{
set the range of the x-axis. Must be one of "calendar," "water," or "climate"
to set the type of year that is shown on the x-axis.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set up x-axis labels. See \code{\link{linearPretty}} for details.
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
A call should be made to \code{setPage} to set up the graphics environment before calling \code{xyPlot}.

To add a plot to the graph created by \code{seasonPlot}, the x-axis data must
be expressed as decimal time.
}
\seealso{
\code{\link{setPage}}, \code{\link{timePlot}}
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY", y = "numeric")}}{
Create a seasonal plot for any valid date data and numeric y-coordinate data.
}

\item{\code{signature(x = "character", y = "numeric")}}{
Create a seasonal plot for date data in the form of month and day, 
like "Jan 01" or "Jaunary 01." Typically used to plot daily mean values.
}
}}
\keyword{methods}
\keyword{hplot}
