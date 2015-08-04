## ----setup, echo=FALSE, warning=FALSE, message=FALSE---------------------
library(knitr)
library(rmarkdown)
library(captioner)
opts_chunk$set(message=FALSE,warning=FALSE,dev="png")
fig_nums <- captioner()

## ------------------------------------------------------------------------
# Load the smwrGraphs package
library(smwrGraphs)
# Generate the random data
set.seed(3636)
X <- rnorm(32)
Y <- X + rnorm(32)

## ----chunk1--------------------------------------------------------------
# Set up the graphics environment, the equivalent call for an on screen
#  device would be setPage("square")

AA.pl <- xyPlot(X, Y)
# Add the median line of Y and annotation
refLine(horizontal = median(Y), current=AA.pl)
addAnnotation(min(X), median(Y), "Median Y", current=AA.pl)


## ----chunk2--------------------------------------------------------------
# Step 1
AA.pl <- xyPlot(X, Y, Plot=list(what="none"))
# Step 2
addGrid(AA.pl)
# Step 3
AA.pl <- addXY(X, Y, Plot=list(what="points"))

## ----chunk3--------------------------------------------------------------
# Create a scatter plot from the X and Y data. The name of the output (AA.pl)
#  is completely arbiutrary, but consistently used through these examples.
AA.pl <- xyPlot(X, Y)
# The addSmooth function will compute the smmothed line and add the plot to the
# graph. Accept all defaults for this example. A very useful additional
# argument would be span for the loess.smooth
AA.pl <- addSmooth(X, Y, current=AA.pl)

## ----chunk4, fig.height=5------------------------------------------------
# Create a scatter plot from the X and Y data. 
AA.pl <- xyPlot(X, Y, Plot=list(what="points", color="black"))
# Create and add the regresion line and 95% confidence intervals
AA.pl <- addSLR(AA.pl)
# The output is discarded in this case becuase an explanation is not created
addCI("SLR", current=AA.pl)
# Create the table and add it to the graph
# Note may actually want to reformat the last p-value so not 0
AA.tbl <- format(round(coefficients(summary(AA.pl$lm)), 4))
AA.tbl <- cbind(" "=c("Intercept", "X"), AA.tbl)
addTable(AA.tbl, where="ul")

## ----chunk5, fig.height=3.5----------------------------------------------
# Create a scatter plot from the X and Y data. 
AA.pl <- xyPlot(X, Y, Plot=list(what="points", color="black"))
# Create and draw an ellipse that covers 90 percent of the data
AA.el <- dataEllipse(X, Y, percent=90)
with(AA.el, addXY(x, y, Plot=list(what="lines", color="darkred"), current=AA.pl))
# Now do the same with a smooth hull
AA.hl <- hull(X, Y, percent=90, smooth=TRUE)
with(AA.hl, addXY(x, y, Plot=list(what="lines", color="magenta"), current=AA.pl))
# Now find the distance from the center of the ellipse and which are greater than 
# the 90th percentile
AA.ds <- mahalanobis(cbind(X,Y), c(mean(X), mean(Y)), var(cbind(X,Y)))
AA.sel <- which(AA.ds > quantile(AA.ds, probs=0.9, type=2))
# Add the labels--the sequence number of the point
labelPoints(X[AA.sel], Y[AA.sel], as.character(AA.sel), current=AA.pl)

