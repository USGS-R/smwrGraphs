\name{addXY-methods}
\docType{methods}
\alias{addXY-methods}
\alias{addXY,ANY,numeric-method}
\title{Methods for Function \code{addXY}}
\description{
Adds an xy-plot to a graph
}
\usage{
\S4method{addXY}{ANY,numeric}(x, y, 
Plot = list(name = "", what = "lines", type = "solid", width = "standard", 
 symbol = "circle", filled = TRUE, size = 0.09, color = "black"), 
current = list(yaxis.log = FALSE, yaxis.rev = FALSE, xaxis.log = FALSE), 
right = FALSE, right.log = FALSE, right.rev = FALSE, right.range = c(NA, NA), 
right.labels = 7, right.title = "")
}

\arguments{
  \item{x}{
the x-axis data. Missing values are permitted, but result in breaks in the plotted data.
}
  \item{y}{
the y-axis data. Missing values are permitted, but result in breaks in the plotted data.
}
  \item{Plot}{
parameters of the plot.
}
  \item{current}{
the current plot information. Typically, this would be the output from one of the graph creation functions like \code{xyPlot}.
}
  \item{right}{
logical: set up new right axis?
}
  \item{right.log}{
logical: log transform right axis?
}
  \item{right.rev}{
logical: reverse right axis?
}
  \item{right.range}{
set right-axis range.
}
  \item{right.labels}{
set up right-axis labels.
}
  \item{right.title}{
the right-axis title.
}
}
\value{
Information about the graph.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY", y = "numeric")}}{
Add a simple line or scatter plot to the current graph.
}
}}
\keyword{methods}
\keyword{aplot}
