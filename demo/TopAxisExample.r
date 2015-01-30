library(smwrGraphs)
library(dataRetrieval)
# Retreive water profile data for USGS lake station 424840088241600 
# LAKE BEULAH AT DEEP HOLE NEAR EAST TROY, WI
# The parameters:
# sampling depth, in meters 000098
# water temperature, in degrees celsius 00010
# dissolved oxygen, in milligrams per liter (mg/L) 00300
LakeB <- readNWISqw("424840088241600", c("00098", "00010", "00300"), 
  startDate="2010-08-19", endDate="2010-08-19")
# The values are returned as character, so convert to numeric
LakeB <- transform(LakeB, p00010=as.numeric(p00010),
   p00098=as.numeric(p00098),  p00300=as.numeric(p00300))
# Set up the graph and axes
setPDF(layout=list(width=4, height=4))
AA.mar <- setTopMargin()
# Plot the data
# The water temperature (W.T.) and dissolved oxygen (D.O.) are on different scales
AA.pl <- with(LakeB, xyPlot(p00010, p00098, Plot=list(what="lines", color="blue"),
  yaxis.rev=TRUE, ytitle="Depth, in meters", xtitle="W.T., in degrees celsius", margin=AA.mar))
## Add annotation at the 7th data point
with(LakeB[7, ], addAnnotation(p00010, p00098, "W.T.",  position="center", current=AA.pl))
AA.pl <- with(LakeB, addXY(p00300, p00098, Plot=list(what="lines", color="black"),
  current=AA.pl, new.axis = "top", new.title="D.O., in mg/L"))
## Add annotation at the 8th data point
with(LakeB[8, ], addAnnotation(p00300, p00098, "D.O.",  justification="right", 
  position="center", current=AA.pl))
graphics.off()
