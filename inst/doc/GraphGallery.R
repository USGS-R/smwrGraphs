## ----setup, echo=FALSE, warning=FALSE, message=FALSE---------------------
library(knitr)
library(captioner)
library(rmarkdown)
opts_chunk$set(message=FALSE,warning=FALSE,dev="png")
fig_nums <- captioner()

## ------------------------------------------------------------------------
# Load the smwrGraphs package
library(smwrGraphs)
# Load the smwrData package and some data
library(smwrData)
data(IonBalance)
data(MiscGW)
# Transform the data. These examples will ignore potassium, fluoride, 
# nitrate, and carbonate. 
PD <- transform(MiscGW, Ca.meq = conc2meq(Calcium, "calcium"),
  Mg.meq = conc2meq(Magnesium, "magnesium"),
  Na.meq = conc2meq(Sodium, "sodium"),
  Cl.meq = conc2meq(Chloride, "chloride"),
  SO4.meq = conc2meq(Sulfate, "sulfate"),
  HCO3.meq = conc2meq(Bicarbonate, "bicarb")) # abbreviations allowed
data(MC11_1993)

## ----chunk1--------------------------------------------------------------
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setPNG()
# Construct means and 95 percent confidence intervals for selected constituents
# in the IonBalance dataset (smwrData)
Stats <- lapply(IonBalance[, c("Ca", "Mg", "Na")],
  function(x) {
    stats <- t.test(x)
    return(c(mean=as.vector(stats$estimate),
      U95=stats$conf.int[2], L95=stats$conf.int[1]))
  })
Stats <- as.data.frame(do.call(rbind, Stats))
Stats$Constituent <- as.factor(row.names(Stats))
# Now the plot and add error bars
AA.pl <- with(Stats, xyPlot(Constituent, mean, yaxis.range=c(0,4),
  Plot=list(name="Mean"),
  ytitle="Concentation in milli-equivalents per liter"))
AA.pl <- with(Stats, addErrorBars(Constituent, L95, U95, 
	Bars=list(name="95 percent confidence interval"), current=AA.pl))
# And the explanation
addExplanation(AA.pl, "ur")

## ----chunk2--------------------------------------------------------------
setPNG()
# Construct the PCA from calcium, magnesium, and sodium and print it
MGW.pp <- princomp(data.matrix(MiscGW[, c("Calcium", "Magnesium", "Sodium")]), 
  cor=TRUE, scores=TRUE)
print(MGW.pp)
# Create the biplot using the default settings
biPlot(MGW.pp)

## ----chunk3--------------------------------------------------------------
setPNG()
# Use the PCA from the previous example. The scores component provides the
# coordinate information for each observation.
# From the biplot, the first axis repesent increasing calcium and the second
# axis increasing difference between sodium and magnesium.
#
# First set up the graph
AA.pl <- with(MGW.pp, xyPlot(scores[,1], scores[,2], Plot=list(what="none"),
  ylabels=0, xlabels=0, ytitle="", xtitle=""))
# Add the axis Labels, these can be placed at the axis minimum, given by
# the "usr" parameter.
addLabel(expression("Increasing Relative Calcium" %->% ""), par("usr")[1],
  "bottom", justification="left", current=AA.pl)
addLabel(expression("Increasing Sodium - Magnesium difference" %->% ""), 
  par("usr")[3],
  "left", justification="left", current=AA.pl)
# Add the Stiff diagrams
AA.pl <- with(MiscGW, addStiff(MGW.pp$scores[,1], MGW.pp$scores[,2],
  width=1, height=0.5, cations=cbind(Calcium, Magnesium, Sodium),
  anions=cbind(Bicarbonate, Sulfate, Chloride), 
  catlabels=c("Ca", "Mg", "Na"), anlabels=c("HCO3", "SO4", "Cl"),
  current=AA.pl))
# Note that there will be overlapping of the Stiff diagram


## ----chunk4--------------------------------------------------------------
# First generate the sequence of x-coordinate values
Xdata <- seq(0, 5, length.out=101)
# Draw the curve
areaPlot(Xdata, dlpearsonIII(Xdata, 0, 1, .5), Areas=list(fillDir="under", 
  fillColors="gray"), xaxis.range=c(0,5))

## ----chunk5, fig.height=7,fig.width=7------------------------------------
setPNG()
# First set up the data in proper form--separate columns for the x- (JULIAN),
# y-(Depth), and z-axis (Temperature) data.
# setPNG("test", width=6, height = 6)
MC11_stack <- reshape(MC11_1993[, c(2, 6:10)], direction="long", varying=list(2:6),
  timevar="Depth", times=c(0.5, 1.0, 1.5, 2.0, 2.5), v.names="Temperature")
# Set up for an explanation
AA.lo <- setLayout(explanation=list(right=1.5))
# Create the contour plot, Note that z is the first argument and no option for
# reversing the sense of the y axis!
setGraph(1, AA.lo)
AA.pl <- with(MC11_stack, contourPlot(Temperature,JULIAN, -Depth,
  Contours=list(name="Soil Temperature", filled=TRUE),
  xaxis.range=c(50, 350), ytitle="Depth in me,ters below land surface",
  xtitle="Julian day, 1993"))
# Add the explanation
setGraph("explanation", AA.lo)
addExplanation(AA.pl)
# graphics.off()


## ----chunk6--------------------------------------------------------------
# Generate the random data.
set.seed(1236)
# Use sorted observations in decimal format for 4 years of data collection
Rdates <- sort(runif(100, 2010, 2014))
# The random, no serial correlation data
Yrand <- scale(rnorm(100))
# Add serial correlation
Yser <- scale(Yrand + c(0, Yrand[-100]) + c(0, 0, Yrand[-c(99,100)])/3)
# Seasonal and temporal lack of fit
Yseas <- scale(Yrand + cos(2*pi*Rdates))
Ytime <- scale(Yrand + seq(-1, 1, length.out=100))
# And the anomaly
Yanom <- scale(Yrand + 0.75*(Rdates > 2011.1 & Rdates < 2011.7))
# Set up for the graphs and create the correlograms
# These use gray90 color and reset the yaxis range to emphasize the line
setPNG()
AA.lo <- setLayout(num.rows=3, num.cols=2)
setGraph(1, AA.lo)
corGram(Rdates, Yrand, Plot=list(color="gray90"), yaxis.range = c(-1, 1))
addTitle("Random data")
setGraph(2, AA.lo)
corGram(Rdates, Yser, Plot=list(color="gray90"), yaxis.range = c(-1, 1))
addTitle("Serial Correlation")
setGraph(3, AA.lo)
corGram(Rdates, Yseas, Plot=list(color="gray90"), yaxis.range = c(-1, 1))
addTitle("Seasonal Lack of Fit")
setGraph(4, AA.lo)
corGram(Rdates, Ytime, Plot=list(color="gray90"), yaxis.range = c(-1, 1))
addTitle("Temporal Lack of Fit")
setGraph(5, AA.lo)
corGram(Rdates, Yanom, Plot=list(color="gray90"), yaxis.range = c(-1, 1))
addTitle("Anomaly")

## ----chunk7--------------------------------------------------------------
setPNG()
# First set up the projection. The preSurface function can be interactive,
# This script sets the projection to A and supresses the request for user input.
AA.pre <- with(MC11_1993, preSurface(JULIAN, c(-2.5, -2.0, -1.5, -1.0, -0.5), 
  cbind(TEMP.2.5, TEMP.2.0, TEMP.1.5, TEMP.1.0, TEMP.0.5), batch="A"))

surfacePlot(AA.pre, xtitle="Julian Day, 1993", ytitle="Depth below landsurface", 
  ztitle="Soil Temperature")

## ----chunk8--------------------------------------------------------------
# Construct the regression
AA.lm <- lm(Mg ~ Ca, data=IonBalance)
# And write the report
setPNG()
reportGraph(summary(AA.lm))


## ----chunk9--------------------------------------------------------------
setPNG()
# Extract the data and assigne rownames based on sample date
CaMg <- data.matrix(IonBalance[, c("Ca", "Mg")])
rownames(CaMg) <- as.character(IonBalance$DATES)
# Construct the distance matrix and the cluster analysis
CaMg.dist <- dist(CaMg)
CaMg.hclust <- hclust(CaMg.dist, method="average")
# Dreaw the dendrogram
dendGram(CaMg.hclust, ytitle="Tree Height")

