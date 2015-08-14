## ----setup, echo=FALSE, warning=FALSE------------------------------------
library(knitr)
opts_chunk$set(message=FALSE, warning=FALSE, dev="png")

## ----message=FALSE-------------------------------------------------------
# Load the smwrGraphs package
library(smwrGraphs)
# Generate a random sample for the box plot
set.seed(27036)
BP <- rchisq(32, 3)
# Generate a small random sample
bp <- rchisq(4, 3)
# Create grouping variables
Gchar <- rep(c("A", "B"), 16)
Gnum <- rep(c(1998, 2002), 16)

## ----fig.cap="The four basic types of box plots."------------------------
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setPNG()
# Set layout for 4 graphs
AA.lo <- setLayout(width=rep(1.25, 4), height=4, xtop=1.5)
# Only need to create the margins once in this case
AA.gr <- setGraph(1, AA.lo)
boxPlot(BP, margin=AA.gr)
addTitle("Truncated")
setGraph(2, AA.lo)
boxPlot(BP, Box=list(type="simple"), margin=AA.gr)
addTitle("Simple")
setGraph(3, AA.lo)
boxPlot(BP, Box=list(type="tukey"), margin=AA.gr)
addTitle("Tukey")
setGraph(4, AA.lo)
boxPlot(BP, Box=list(type="extended"), margin=AA.gr)
addTitle("Extended")


## ----fig.cap="Box plot with explanation."--------------------------------
setPNG()
# Set layout for 1 graph and an explanation
AA.lo <- setLayout(width=1.5, height=4, explanation=list(right=1.5))
# Only need to create the margins once in this case
AA.gr <- setGraph(1, AA.lo)
AA.bp <- boxPlot(BP, margin=AA.gr)
setGraph("explanation", AA.lo)
addExplanation(AA.bp, title=expression(bold("Truncated Boxplot")))


## ----fig.cap="Box plot variations.", fig.height=5------------------------
setPNG()
# The color gray80 is a very light gray and works well for the fill
boxPlot(BP, bp, Box=list(fill="gray80", width=1.0), xlabels=c("Big", "Small"))


## ----fig.cap="A grouped box plot."---------------------------------------
setPNG()
# Accept default graph size for this example
boxPlot(BP, group=Gchar)

## ----fig.cap="A box plot grouped by a numeric variable."-----------------
setPNG()
# Accept default graph size for this example
boxPlot(BP, group=Gnum)


