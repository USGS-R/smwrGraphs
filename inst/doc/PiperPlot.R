## ----setup, echo=FALSE, warning=FALSE, message=FALSE---------------------
library(knitr)
library(captioner)
library(rmarkdown)
opts_chunk$set(message=FALSE,warning=FALSE,dev="png")
fig_nums <- captioner()

## ------------------------------------------------------------------------
# Load the smwrGraphs package
library(smwrGraphs)
# Generate a random sample for the ternary diagram
set.seed(2727)
# Ternary diagram data
X <- runif(25, .1, 1.)
Y <- runif(25, .1, .8)
Z <- runif(25, .3, 1.)
# Get the selected groundwqater quality date from Hem
library(smwrData)
data(MiscGW)

## ----chunk1, fig.height=7, fig.width=7-----------------------------------
# Transform the data. This example will ignore potassium, fluoride, and nitrate
# (carbonate is either 0 or missing and will also be ignored).
PD <- transform(MiscGW, Ca.meq = conc2meq(Calcium, "calcium"),
                    Mg.meq = conc2meq(Magnesium, "magnesium"),
		    Na.meq = conc2meq(Sodium, "sodium"),
		    Cl.meq = conc2meq(Chloride, "chloride"),
		    SO4.meq = conc2meq(Sulfate, "sulfate"),
		    HCO3.meq = conc2meq(Bicarbonate, "bicarb")) 
# abbreviations allowed in the cal to conc2meq
# The row name identifies the sample source, create a column
PD$SS <- row.names(PD)

# The minimum page size for a Piper plot is 7 inches. No check is made,
#  but the axis title spacings require a graph area of at least 6 inches.

# For this example, a separate graph area for an explanation is not needed
#  because there are only 4 groups (individuals).
AA.pl <- with(PD, piperPlot(Ca.meq, Mg.meq, Na.meq, 
    Cl.meq, HCO3.meq, SO4.meq,
  Plot=list(name=SS, color=setColor(SS)),
  zCat.title = "Sodium",
  xAn.title = "Chloride",
  yAn.title = "Bicarbonate"))
addExplanation(AA.pl, where="ul", title="")


## ----chunk2, fig.height=7, fig.width=7-----------------------------------
# Create the empty Piper plot
AA.pl <- with(PD, piperPlot(Ca.meq, Mg.meq, Na.meq, 
    Cl.meq, HCO3.meq, SO4.meq,
  Plot=list(what="none"),
  ticks = TRUE,
  zCat.title = "Sodium",
  xAn.title = "Chloride",
  yAn.title = "Bicarbonate"))
# Fill in the symbols in the triangular graphs, do not overwrite AA.pl
with(AA.pl, addPiper(xCat=cations$x, yCat=cations$y, xAn=anions$x, yAn=anions$y,
  xPip=NA, yPip=NA, # Missing values are not plotted
  Plot=list(size=.05), current=AA.pl))
# Compute a measure of the ionic strength
PD <- transform(PD, TotalCat=Ca.meq + Mg.meq + Na.meq)
# Compute the symbol size (mean diameter is .2 inch)
PD.size <- 0.2*sqrt(PD$TotalCat)/mean(sqrt(PD$TotalCat))
# Now add the scaled circles to the middle plot
with(AA.pl, addPiper(xCat=NA, yCat=NA, xAn=NA, yAn=NA, 
  xPip=piper$x, yPip=piper$y,
  Plot=list(size=PD.size, filled=FALSE), current=AA.pl))

## ----chunk3, fig.height=3.5, fig.width=3.5-------------------------------
# Accept all defaults
AA.pl <- ternaryPlot(X, Y, Z)
# Use the chull function to extract the points that define the 
# convex hull of the data.
AA.pts <- chull(AA.pl$x, AA.pl$y)
# Close it
AA.pts[length(AA.pts) + 1] <- AA.pts[1]
# Select those points and draw the hull
addTernary(X[AA.pts], Y[AA.pts], Z[AA.pts],
  Plot=list(what="lines"), current=AA.pl)


## ----chunk4, eval=FALSE--------------------------------------------------
#  
#  AA.lo <- setLayout(height=3.5, explanation=list(bottom=1.1))
#  setGraph(1, AA.lo)
#  # Accept all defaults, but subset the data for the small graph size
#  AA.pl <- with(PD, stiffPlot(cbind(Ca.meq, Mg.meq, Na.meq),
#           cbind(Cl.meq, SO4.meq, HCO3.meq), ylabels=SS))
#  setGraph("explanation", AA.lo)
#  addExplanation(AA.pl)

