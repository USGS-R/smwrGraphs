\name{histGram}
\alias{histGram}
\alias{histGram.default}
\title{
Histogram
}
\description{
Create either a frequency of density histogram.
}
\usage{
histGram(x, breaks = "Sturges", Hist = list(), 
 yaxis.range = c(NA, NA), ylabels = 7, xlabels = 7, 
 xtitle = "", ytitle = "Auto", caption = "", margin = c(NA, NA, NA, NA), ...)
\method{histGram}{default}(x, breaks = "Sturges", Hist = list(type="frequency",
  fill=FALSE, boundary="lower", line.color="black", fill.color="gray80"),
 yaxis.range=c(NA,NA), ylabels=7, xlabels=7,
 xtitle=deparse(substitute(x)), ytitle="Auto",
 caption="", margin=c(NA, NA, NA, NA), ...)
}
\arguments{
  \item{x}{
a numeric vector to create the histogram
}
  \item{breaks}{
any valid value for \code{\link{hist}}. See \bold{Details}.
}
  \item{Hist}{
Controls for the histogram.
}
  \item{yaxis.range}{
set the range for the y axis, the first value must be 0.
}
  \item{ylabels}{
the approximate number of labels for the y axis.
}
  \item{xlabels}{
the approximate number of labels for the x axis.
}
  \item{xtitle}{
x-axis title for \code{x} data.
}
  \item{ytitle}{
the y-axis title, "Frequency" for a frequency histogram, "Density" for a density histogram.
}
  \item{caption}{
the figure caption.
}
  \item{margin}{
set up the plot area margins.
}
  \item{\dots}{
additional arguments for other methods.
}
}
\details{
To set the x-axis range, you must specify numeric breaks that span the complete range of \code{x}.
}
\value{
Information about the graph.
}
\references{
Helsel and Hirsch, Chapter 2
}
\note{
A call should be made to \code{setPage} to set up the graphics environment before calling \code{histGram}.
}

\seealso{
\code{\link{ecdfPlot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
## output vignettes to screen
}
\keyword{hplot}

