\name{USGSPretty}
\alias{linearPretty}
\alias{logPretty}
\alias{transPretty}
\title{
Pretty Axis
}
\description{
Construct information for making a "pretty" numeric axis.
}
\usage{
linearPretty(x, hard = FALSE, labels = "Auto", style = "numeric", extend.pct = 0)
logPretty(x, hard = FALSE, labels = "Auto", style = "numeric", extend.pct = 0)
transPretty(x, hard = FALSE, labels = 11, style = "none", func = log, Ifunc = exp, ...)
}

\arguments{
  \item{x}{
data defining the range to be plotted on the axis. Missing value are permitted, but ignored.
}
  \item{hard}{
use the minimum and maximum of \code{x} as the fixed range of the axis?
}
  \item{labels}{
either "Auto," which lets the function decide how many labels, the approximate number of labels,
or the actual labels to use.
}
  \item{style}{
a character string indicating the style of the axis labels if they are not specifically listed in \code{labels}.
}
  \item{extend.pct}{
extend the axis range by \code{extend.pct}.
}
  \item{func}{
the forward transform function.
}
  \item{Ifunc}{
the bcakward (inverse) transform function.
}
  \item{\dots}{
additional arguments to \code{func} and \code{Ifunc}.
}
}
\value{
Information about the axis lables.
}
\seealso{
\code{\link{areaPlot}}, \code{\link{boxPlot}}, \code{\link{colorPlot}}, \code{\link{areaPlot}},
\code{\link{dotPlot}}, \code{\link{ecdfPlot}}, \code{\link{probPlot}}, \code{\link{qqPlot}},
\code{\link{scalePlot}}, \code{\link{splomPlot}}, \code{\link{timePlot}}, \code{\link{transPlot}},
\code{\link{xyPlot}}
}
\keyword{ dplot }
