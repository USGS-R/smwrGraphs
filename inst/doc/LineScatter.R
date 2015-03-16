### R code from vignette source 'LineScatter.Rnw'

###################################################
### code chunk number 1: LineScatter.Rnw:19-32
###################################################
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


###################################################
### code chunk number 2: LineScatter.Rnw:42-49
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot01", 6 ,6)
xyPlot(X, Y, Plot=list(color="darkblue"))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 3: LineScatter.Rnw:62-70
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot02", 6 ,6)
with(KlamathTP, timePlot(sample_dt, TP_ss, Plot=list(what="points", color="darkblue"),
  yaxis.range=c(0,1.5)))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 4: LineScatter.Rnw:83-90
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot03", 6 ,6)
with(KlamathTP, seasonPlot(sample_dt, TP_ss, yaxis.range=c(0,1.5)))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 5: LineScatter.Rnw:103-112
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot05", 6 ,4)
# Create the regular series of observations
AA.rs <- with(KlamathTP, regularSeries(TP_ss, sample_dt, begin="1972-01-01", end="1980-01-10"))
seriesPlot(AA.rs$Value, yaxis.range=c(0,1.5), xlabels=month.USGS)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 6: LineScatter.Rnw:126-135
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot06", 6 ,4)
# Accept the default colors for groups.
AA.pl <- colorPlot(X, Y, color=Z)
addExplanation(AA.pl, where="ul", title="", box.off=FALSE)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 7: LineScatter.Rnw:148-158
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot07", 4, 4)
# Create the y-axis data as a factor with levels sorted by x
Z12f <- factor(Z12, levels=Z12[order(X12)])
# Plot the results
dotPlot(X12, Z12f)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 8: LineScatter.Rnw:171-179
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot08", 6 ,6)
# Plot Calcium and Magnesium
with(IonBalance, scalePlot(Ca, Mg, Plot=list(what="points")))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 9: LineScatter.Rnw:191-200
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("lsplot09", 6 ,6)
# Plot Calcium and Magnesium and Sodium
AA.lo <- with(IonBalance, setSplom(num.variables=3, touching=FALSE))
with(IonBalance, splomPlot(cbind(Ca, Mg, Na), Panel=list(line="slr"), layout=AA.lo))
# Required call to close PDF output graphics
graphics.off()


