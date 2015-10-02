### R code from vignette source 'BoxPlots.Rnw'

###################################################
### code chunk number 1: BoxPlots.Rnw:33-43
###################################################
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


###################################################
### code chunk number 2: BoxPlots.Rnw:61-82
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("boxplot01", 6 ,6)
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
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 3: BoxPlots.Rnw:97-106
###################################################
setSweave("boxplot02", 6 ,6)
# Set layout for 1 graph and an explanation
AA.lo <- setLayout(width=1.5, height=4, explanation=list(right=1.5))
# Only need to create the margins once in this case
AA.gr <- setGraph(1, AA.lo)
AA.bp <- boxPlot(BP, margin=AA.gr)
setGraph("explanation", AA.lo)
addExplanation(AA.bp, title="Truncated Boxplot")
graphics.off()


###################################################
### code chunk number 4: BoxPlots.Rnw:132-137
###################################################
setSweave("boxplot03", 6 ,5)
# The figure is set to a heigth of 5 inches to fit on the page.
# The color gray80 is a very light gray and works well for the fill
boxPlot(BP, bp, Box=list(fill="gray80", width=1.0), xlabels=c("Big", "Small"))
graphics.off()


###################################################
### code chunk number 5: BoxPlots.Rnw:153-157
###################################################
setSweave("boxplot04", 6 ,6)
# Accept default graph size for this example
boxPlot(BP, group=Gchar)
graphics.off()


###################################################
### code chunk number 6: BoxPlots.Rnw:174-178
###################################################
setSweave("boxplot05", 6 ,6)
# Accept default graph size for this example
boxPlot(BP, group=Gnum)
graphics.off()


