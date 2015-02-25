library(smwrGraphs)
library(dataRetrieval)
# Retreive discharge measurements for USGS streamgage 05127500 
# BASSWOOD RIVER NEAR WINTON, MN
Basswood <- readNWISmeas("05127500", startDate="2008-10-01", endDate="2014-09-30")
# The rating number for this period was 17.0, whihc was a double offset
# rating curve which rated flow from a gage height of 1.6 to 3.3 feet to
# a flow of 44 to 881 cubic feet per scond with an offset of 0.6 and rated 
# flow from 3.3 to 10.0 to a flow of 881 to 16,000 with an offset of 1.9
# This graph will display only the lower part of the curve
# Set up the graph and axes
setPDF(layout=list(width=8, height=8), basename="Basswood")
# Define the forward and inverse log-offset functions
logoff <- function(x, off) log(x - off)
Ilogoff <- function(x, off) exp(x) + off
# Plot the measurements where the flow was less than 900
AA.pl <- with(subset(Basswood, discharge_va < 900),
  transPlot(discharge_va, log, exp,
    y=gage_height_va, ytrans=logoff, yinv=Ilogoff, ytargs=list(off=0.6),
    xaxis.range=c(100, 900)))
# Draw the rating curve
addXY(c(44,881), c(1.6, 3.3), current=AA.pl)
graphics.off()
