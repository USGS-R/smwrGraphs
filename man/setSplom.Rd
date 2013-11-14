\name{setSplom}
\alias{setSplom}

\title{
Scatter Plot Matrix
}
\description{
Set up a scatter plot matrix. 
}
\usage{
setSplom(size = NULL, num.variables, show.all = FALSE, touching = TRUE, explanation = NULL, 
ymargin = 3.5)
}

\arguments{
  \item{size}{
the width and height of the entire graph area, exclusive
of explanation. If NULL, then use minimum of figure width and height.
}
  \item{num.variables}{
the number of variables to plot.
}
  \item{show.all}{
show the full grid? Otherwise only lower triangular graphs.
}
  \item{touching}{
should individual graphs touch? Otherwise a small gap separates individual graphs.
}
  \item{explanation}{
a description of where to place the explanation if needed. See \bold{Details}.
}
  \item{ymargin}{
the left-margin for the plot area for the left column of graphs.
}
}
\details{
If an explanation is needed, then \code{explanation} is used to indicate
where the explanation is to be placed. The explanation can be placed either to the right of the grid 
of graphs, at the bottom of the grid, or in one of the grid cells.\cr
To place an explanation to the right of the graphs, \code{explantion} should be set to \code{list(right=ewid)}, 
where \code{ewid} is the width of the explanation. In this case, the total of \code{width} and \code{ewid} must
be less that the total available for the page.\cr
To place an explanation at the bottom of the graphs, \code{explantion} should be set to \code{list(bottom=ehei)},
where \code{ehei} is the height of the explanation. In this case, the total pf \code{height} and \code{ehei} must
be less than the total available for the page.\cr
To place an explanation within a cell of the grid, \code{explantion} should be set to \code{list(grid=enum)},
where \code{enum} is the cell number in the grid. Cell numbers are sequential starting in the upper left
and increasing by column. In this case \code{num.graphs} must be set to some number less than \code{num.cols}
times \code{num.rows}.\cr
}
\value{
a list like \code{setLayout} with three additional components: \code{show.all}, \code{touching}, 
and \code{num.variables} from the call to \code{setSplom}.
}
\seealso{
\code{\link{setLayout}}
}
\keyword{dplot}
