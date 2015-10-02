### R code from vignette source 'ProbabilityPlots.Rnw'

###################################################
### code chunk number 1: ProbabilityPlots.Rnw:33-41
###################################################
# Load the smwrGraphs package
library(smwrGraphs)
# Generate the random data
set.seed(2736)
Xnorm <- rnorm(32)
Xlogn <- rlnorm(32)
Xmix <- exp(c(rnorm(15), rnorm(15, 0.5))) + .5
Xbig <- rnorm(100)


###################################################
### code chunk number 2: ProbabilityPlots.Rnw:52-62
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("probplot01", 6 ,6)
# Create the graph. Note that by default, the x-axis is log-transformed and
#  requires strictly positive data. Setting xaxis.log to
#  FALSE relaxes that requirement.
ecdfPlot(Xmix)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 3: ProbabilityPlots.Rnw:83-90
###################################################
setSweave("probplot02", 6 ,6)
# For the normal distribution, the mean and sd arguments are optional, if
#  supplied, then a line for the fitted distribution is drawn. The default
#  setting for yaxis.log is TRUE, so the logs of the data are required for the
#  mean and standard deviation.
probPlot(Xlogn, mean=mean(log(Xlogn)), sd=sd(log(Xlogn)))
graphics.off()


###################################################
### code chunk number 4: ProbabilityPlots.Rnw:106-111
###################################################
setSweave("probplot03", 6 ,6)
# Accept all of the defaults, the line is based on the mean and the standard
#  deviation of the data.
qqPlot(Xnorm)
graphics.off()


###################################################
### code chunk number 5: ProbabilityPlots.Rnw:128-132
###################################################
setSweave("probplot04", 6 ,6)
# Accept all of the defaults.
qqPlot(Xnorm, Xbig)
graphics.off()


###################################################
### code chunk number 6: ProbabilityPlots.Rnw:150-154
###################################################
setSweave("probplot05", 6 ,6)
histGram(Xbig)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 7: ProbabilityPlots.Rnw:164-174
###################################################
# Compute the density
Xbig.den <- density(Xbig)
range(Xbig.den$x)
setSweave("probplot06", 6, 4)
# Set type to density and xaxis range to -4, 3.5 by setting breaks
histGram(Xbig, breaks=seq(-4, 3.5, by=.5), Hist=list(type="density"))
# Add the density line, the defaults all work so current arg not needed
with(Xbig.den, addXY(x, y))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 8: ProbabilityPlots.Rnw:193-235
###################################################
# Set up the output
setSweave("probplot07", 6, 8)
# Allocate 3 graphs
AA.lo <- setLayout(num.rows=3)
# Figure 7A
setGraph(1, AA.lo)
AA.pl <- ecdfPlot(Xmix)
# Add another plot, simply Xmix + 0.5
Xadd <- Xmix + 0.5
# Replicate the smallest values and sort the data
Xadd <- c(min(Xadd), sort(Xadd))
# The y-axis coordinates are the sequence from 0 to 1 with legnth matching the data
addXY(Xadd, seq(0, 1, length.out=length(Xadd)), 
  Plot=list(what="stairstep", color="blue"), current=AA.pl)
addTitle(Heading="A")
# Figure 7B
setGraph(2, AA.lo)
AA.pl <- probPlot(Xlogn)
# Add another plot, simply Xlogn + 0.5
Xadd <- Xlogn + 0.5
# Sort the data 
Xadd <- sort(Xadd)
# The x-axis coordinate are the plotting positions
addXY(ppoints(Xadd, a=0.4), Xadd,
  Plot=list(what="points", color="blue"), current=AA.pl)
addTitle(Heading="B")
# Figure 7C
setGraph(3, AA.lo)
AA.pl <- qqPlot(Xnorm)
# Add another plot, simply subset Xbig
Xadd <- sample(Xbig, 20)
# Sort the data 
Xadd <- sort(Xadd)
# The x-axis coordinate are the plotting positions
addXY(qnorm(ppoints(Xadd, a=0.4)), Xadd,
  Plot=list(what="points", color="blue"), current=AA.pl)
# And add the line representing the mean and sd
refLine(coefficients=c(mean(Xadd), sd(Xadd)),
  Plot=list(color="blue"), current=AA.pl)
addTitle(Heading="C")
# Required call to close PDF output graphics
graphics.off()


