\name{setMargin}
\alias{setMargin}

\title{
Graph Margins
}
\description{
Set the margins for the plot area (support function).
}
\usage{
setMargin(margin, yax, aux.label = FALSE, caption = TRUE)
}

\arguments{
  \item{margin}{
incomplete plot margin specification, generally computed by \code{setGraph}.
}
  \item{yax}{
the y-axis information from a "pretty" function, required if the second entry of \code{margin} is \code{NA}.
}
  \item{aux.label}{
second level of x-axis labels?
}
  \item{caption}{
account for figure caption?
}
}
\value{
Complete plot margin specification.
}
\seealso{
\code{\link{setPage}}, \code{\link{setGraph}}, \code{\link{setLayout}}
}
\keyword{ dplot }
