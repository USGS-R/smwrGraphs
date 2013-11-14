\name{copyDemo}
\alias{copyDemo}
\title{
Copy a Demo File
}
\description{
Copies a demo file from the source package to a file.
}
\usage{
copyDemo(topic, package = "USGSwsGraphs", file)
}
\arguments{
  \item{topic}{
the name of the topic, must be a character string.
}
  \item{package}{
the package name, must be a character string.
}
  \item{file}{
the target file name, must be a character string. If missing, then
the file name is created from the \code{topic} name.
}
}

\value{
Either the name of the target file or \code{NULL} if the copy failed.
}

\seealso{
\code{\link{demo}}
}
\examples{
\dontrun{copyDemo("HydroPrecip")}
}
\keyword{utilities}

