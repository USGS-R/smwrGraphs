## ----setup, echo=FALSE, warning=FALSE, message=FALSE---------------------
library(knitr)
library(rmarkdown)
library(captioner)
opts_chunk$set(message=FALSE,warning=FALSE,dev="png")
fig_nums <- captioner()

## ------------------------------------------------------------------------
# Load the smwrGraphs package
library(smwrGraphs)
# Generate random samples for the examples.
set.seed(2576)
X <- runif(33)
Y <- runif(33)
Z <- rep(c("A", "B", "C"), 11)
X12 <- X[1:12]
Z12 <- LETTERS[1:12]
# Load the smwrData package
library(smwrData)
data(IonBalance)
data(KlamathTP)

## ----chunk1--------------------------------------------------------------
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setPNG()
xyPlot(X, Y, Plot=list(color="darkblue"))


## ----chunk2--------------------------------------------------------------
setPNG()
with(KlamathTP, timePlot(sample_dt, TP_ss, Plot=list(what="points", color="darkblue"),
  yaxis.range=c(0,1.5)))

## ----chunk3--------------------------------------------------------------
setPNG()
with(KlamathTP, seasonPlot(sample_dt, TP_ss, yaxis.range=c(0,1.5)))


## ----chunk5, fig.height=4------------------------------------------------
setPNG()
# Create the regular series of observations
AA.rs <- with(KlamathTP, regularSeries(TP_ss, sample_dt, begin="1972-01-01", 
  end="1980-01-10"))
seriesPlot(AA.rs$Value, yaxis.range=c(0,1.5), xlabels=month.USGS)

## ----chunk6, fig.height=4------------------------------------------------
setPNG()
# Accept the default colors for groups.
AA.pl <- colorPlot(X, Y, color=Z)
addExplanation(AA.pl, where="ul", title="", box.off=FALSE)

## ----chunk7, fig.height=4, fig.width=4-----------------------------------
setPNG()
# Create the y-axis data as a factor with levels sorted by x
Z12f <- factor(Z12, levels=Z12[order(X12)])
# Plot the results
dotPlot(X12, Z12f)

## ----chunk8--------------------------------------------------------------
setPNG()
# Plot Calcium and Magnesium
with(IonBalance, scalePlot(Ca, Mg, Plot=list(what="points")))

## ----chunk9--------------------------------------------------------------
setPNG()
# Plot Calcium and Magnesium and Sodium
AA.lo <- with(IonBalance, setSplom(num.variables=3, touching=FALSE))
with(IonBalance, splomPlot(cbind(Ca, Mg, Na), Panel=list(line="slr"), layout=AA.lo))


