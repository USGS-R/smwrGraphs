\name{boxPlot}
\alias{boxPlot}
\alias{boxPlot.list}
\alias{boxPlot.data.frame}
\alias{boxPlot.numeric}
\title{
Box Plot
}
\description{
Produces a truncated, simple, or Tukey box plot.
}
\usage{
boxPlot(..., group = NULL, Box = list(type = "truncated", show.counts = TRUE, 
nobox = 5, width = "Auto", fill="none", truncated = c(10, 90)), yaxis.log = FALSE, 
yaxis.range = c(NA, NA), ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE, 
xtitle = "", ytitle = "", caption = "", margin = c(NA, NA, NA, NA))

\method{boxPlot}{list}(..., group = NULL, Box = list(type = "truncated", show.counts = TRUE, 
nobox = 5, width = "Auto",  fill="none", truncated = c(10, 90)), 
yaxis.log = FALSE, yaxis.range = c(NA, NA), ylabels = "Auto", xlabels = "Auto", 
xlabels.rotate = FALSE, xtitle = "", ytitle = "", caption = "", margin = c(NA, NA, NA, NA))

\method{boxPlot}{data.frame}(..., group = NULL, Box = list(type = "truncated", show.counts = TRUE, 
nobox = 5, width = "Auto",  fill="none", truncated = c(10, 90)), yaxis.log = FALSE, 
yaxis.range = c(NA, NA), ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE, xtitle = "", 
ytitle = "", caption = "", margin = c(NA, NA, NA, NA))

\method{boxPlot}{numeric}(..., group = NULL, Box = list(type = "truncated", show.counts = TRUE, 
nobox = 5, width = "Auto",  fill="none", truncated = c(10, 90)), yaxis.log = FALSE, 
yaxis.range = c(NA, NA), ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE, xtitle = "", 
ytitle = "", caption = "", margin = c(NA, NA, NA, NA))
}

\arguments{
  \item{\dots}{
the data to plot.
}
  \item{group}{
create groups of a single numeric vector. Invalid for any kind of object other than numeric.
}
  \item{Box}{
control parameters for the box. See \bold{Details}.
}
  \item{yaxis.log}{
logical: log transform y axis?
}
  \item{yaxis.range}{
set y-axis range.
}
  \item{ylabels}{
set up y-axis labels. See \code{\link{linearPretty}} for details.
}
  \item{xlabels}{
set up x-axis labels.
}
  \item{xlabels.rotate}{
rotate x-axis labels 90 degrees?
}
  \item{xtitle}{
the x-axis title
}
  \item{ytitle}{
the y-axis title
}
  \item{caption}{
the figure caption
}
  \item{margin}{
set up the plot area margins.
}
}
\details{
If group is numeric, then the boxes will be plotted along a continuous numeric axis. Otherwise
the x-axis will be discrete groups.\cr
\code{Box} is a list with these components:
\describe{
	\item{"type"}{the type of boxtplot: "simple" the whiskers extend 
	to the minimum and maximum of the data, "truncated" the whisker extend to percentiles defined by
	\code{truncated}, "tukey" the standard "Tukey" boxplot, and "extended" the whisker extend to percentiles defined by
	\code{truncated} and values outside of that range are shown;}
	\item{"show.counts"}{show the number of observations used to compute the boxplot statistics;}
	\item{"nobox"}{only individual values are shown if the number of observations is less than or equal to this value;}
	\item{"width"}{the width of the box, in inches;}
	\item{"fill"}{The color of the filled box or "none" for no fill;}
	\item{"truncated"}{the percentiles to use for the truncated boxplot.}
	}
}
\value{
Information about the graph.
}
\note{
  A call should be made to \code{setPage} to set up the graphics
  environment before calling \code{boxPlot}.\cr
  If \code{yaxis.log} is set to \code{TRUE}, then the quartiles and
  interquartile range are computed from the log-transformed values
  rather then the untransformed values, which is common for other box
  plots. Those computations are in agreement with the box plots
  generated in the QWGRAPH component of the QWDATA module in the
  National Water Information System (NWIS) described by Dennis Helsel's
  1989 Branch of Systems Analysis Technical Memorandum No. 89.01,
  available online
 at
  \url{http://water.usgs.gov/admin/memo/BSA/BSA89.01.pdf}. Those
  computations have a significant effect on the appearance the whiskers
  and outside values of the Tukey box plot and are motivated by the
  general assumption of a log-normal distribution for most
  water-quality constituents.
  
}
\seealso{
\code{\link{setPage}}, \code{\link{dotPlot}}
}
\keyword{hplot}
