\name{setRtMargin}
\alias{setRtMargin}

\title{
Set Right Margin
}
\description{
Set the right margin for graphs with secondary right axes. Used After setting up the graphics environment, but before the call the the high-level graphics 
function to allocate space for an additional right-axis label.
}
\usage{
setRtMargin(y, margin = c(NA,NA,NA,NA), right.labels = 7, 
    right.log = FALSE, right.range = c(NA, NA))
}

\arguments{
  \item{y}{
the secondary y-axis data to be plotted, missing values are permitted and are ignored.
}
  \item{margin}{
incomplete plot margin specification, generally computed by \code{setGraph}.
}
  \item{right.labels}{
set up right-axis labels; the approximate number of labels.
}
  \item{right.log}{
logical: log transform right axis?
}
  \item{right.range}{
set right-axis range.
}
}
\details{
The values for \code{right.labels}, \code{right.log}, and \code{right.range}
should be set exactly as in the call to \code{addXY}.
}
\value{
The updated margin; only the right margin value is changed.
}
\seealso{
\code{\link{setLayout}}, \code{\link{setGraph}}, \code{\link{addXY-methods}}
}
\keyword{ dplot }
