\name{addTitle}
\alias{addTitle}
\title{
Add a Title
}
\description{
Adds a title to a graph.
}
\usage{
addTitle(Main = "", Heading = "", Justification = "left", Bold = TRUE)
}
\arguments{
  \item{Main}{
the title.
}
  \item{Heading}{
The title header, generally a single letter. See \bold{Details}  
}
  \item{Justification}{
specifies the location of the title, must be one of "left," "center," or "right."
}
  \item{Bold}{
logical: should the title be in bold face type?
}
}
\details{
  If only \code{Heading} is non blank, then the title is a single letter in bold
italics. If both \code{Heading} and \code{Main} are non blank, then the title is a single
letter followed by a period in bold italics followed by \code{Main} in bold if \code{Bold}
is \code{TRUE}.
}
\value{
Nothing is returned.
}
\seealso{
\code{\link{addCaption}}, \code{\link{addAnnotation}},  \code{\link{addTable}}
}
\keyword{aplot}

