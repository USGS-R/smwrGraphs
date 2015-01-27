library(smwrGraphs)
library(smwrData)
data(MenomineeMajorIons)
# Subset the data to 1993 and later
MMI <- subset(MenomineeMajorIons, sample.dt > "1993-01-01")
# Set up the time-series plot for bicarbonate and calcium
setPDF()
AA.mar <- with(MMI, setRtMargin(Calcium, right.log = FALSE))
# Plot the data
# It is difficult to show the time-series correlation between bicarbonate and calcium
# if they are shown on the same axis, so use left-and right axes.
AA.pl <- with(MMI, timePlot(sample.dt, HCO3, Plot=list(name="Bicarbonate", what="points"),
  yaxis.log=TRUE, ytitle="Bicarbonate Concentration, in mg/L", margin=AA.mar))
AA.pl <- with(MMI, addXY(sample.dt, Calcium, Plot=list(name="Calcium", what="points", color="red"),
  current=AA.pl, new.axis = "right", new.log = TRUE, new.title="Calcium Concentration, in mg/L"))
## Add an explanation
addExplanation(AA.pl, "ul")
graphics.off()
