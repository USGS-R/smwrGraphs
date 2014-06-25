\name{setPlot}
\alias{setPlot}

\title{
Plot Parameters
}
\description{
Set the control parameters for a plot (support function).
}
\usage{
setPlot(current, name = "", what = "lines", type = "solid", width = "standard", 
symbol = "circle", filled = TRUE, size = 0.09, color = "black", area.color = NA, 
area.border = NA)
}

\arguments{
  \item{current}{
list containing the current or those requested by the user.
}
  \item{name}{
the name of the object plotted; used in the explanation.
}
  \item{what}{
what to plot, see \bold{Details}.
}
  \item{type}{
the line type, if drawn, must be one of "solid," "dashed," or "dotted."
}
  \item{width}{
the width of the line, if drawn, must be one of "standard;" "color," a little wider than "standard;"
"bold," twice as wide as "standard;" or "hairline."
}
  \item{symbol}{
the symbol to plot, if drawn, see \bold{Details}.
}
  \item{filled}{
if a symbol is drawn, fill with solid color?
}
  \item{size}{
the size of the symbol, in incehs, if drawn.
}
  \item{color}{
the color of the symbol or line.
}
  \item{area.color}{
the color of a shaded area, required for completeness.
}
  \item{area.border}{
the boundary color of a shaded area, required for completeness.
}
}
\details{
The value for \code{what} must be one of:
\describe{
	\item{"points"}{symbols only,}
	\item{"lines"}{lines only,}
	\item{"both"}{lines connecting symbols with a small gap,}
	\item{"overlaid"}{lines connecting symbols,}
	\item{"stairstep"}{horizontal line to next x value with a vertical line to the y value,}
	\item{"vertical"}{vertical lines from the y-axis only.}
	\item{"none"}{draw nothing}
	}
The value for \code{symbol} must be one of:
\describe{
	\item{"circle"}{an open circle or filled, depending on \code{filled},} 
	\item{"uptri"}{an open up pointing triangle or filled, depending on \code{filled},} 
	\item{"plus"}{a plus sign (never filled),}
	\item{"x"}{an x (never filled),}
	\item{"diamond"}{an open diamond shape or filled, depending on \code{filled},}
	\item{"downtri"}{an open down pointing triangle or filled, depending on \code{filled},} 
	\item{"square"}{an open square or filled, depending on \code{filled},} 
	\item{"dot"}{a very small dot (never filled),}
	\item{"+"}{a plus sign (never filled),}
	\item{"none"}{no symbol or line.}
}
}
\value{
A list like \code{current}, but with the defaults supplied for any missing component.
}
\seealso{
\code{\link{xyPlot}}, \code{\link{timePlot}}, \code{\link{qqPlot}}, \code{\link{piperPlot}}, \code{\link{probPlot}},
\code{\link{colorPlot}}
}
\keyword{ hplot }
