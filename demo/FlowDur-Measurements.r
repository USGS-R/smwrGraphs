# The flow duration curve is a variation on the empirical distribution fucntion plot
#  where the percent or probability of exceedance is plotted on the x-axis and
#  streamflow on the y-axis. In most cases, the x-axis is treated as an uniform
#  distribution and the y-axis is on a log-scale.
# This demo requires an internet connection.
# Get the streamflow data. Use the time frame between 1970-10-01 and 2010-09-30,
#  water years 1971 through 2010.
library(dataRetrieval)
# The default data are daily streamflow
LPR <- readNWISdv("05245100", "00060", startDate="1970-10-01", endDate="2010-09-30")
# Make more readable name for flow
LPR <- renameNWISColumns(LPR)
# Get streamflow measurements for the same time period
LPR.MEAS <- readNWISmeas("05245100", startDate="1970-10-01", endDate="2010-09-30")
# The column of interest is discharge_va
# Get the library and set up the page
library(smwrGraphs)
# landscape generally is preferred format, but the call to setGD should work on any environment.
setGD("FlowDur") 
AA.plt <- probPlot(LPR$Flow, distribution='uniform', 
    FLIP=T, CDF=T, Plot=list(name="Daily Streamflow", what="lines"),
	xlabels=100*(0:10)/10, ytitle="Streamflow, in cubic feet per second", 
	xtitle="Percent of Time Streamflow is Exceeded",
	caption="Streamflow duration and measured streamflow Long Prarie River water years 1971 to 2010")
# AA.plt is a list with components x and y and others. The component y is the x-argument to the probPlot
# call--in this case LPR$Flow, and because the y-axis is in log scale, the values
# were log10 transformed. Both are also sorted by x. The component x is plotting postion corresponding
# to the value of x.
#
# The interpLine function can interpolate values along a line that has been drawn or any list
#  that has both an x and y component. It can interpolate x values from y if y is sorted and 
#  y values from x if x is sorted.
# Add a column to LPR.MEAS that represents the cumulative probability of non exceedance.
# The warn argument can be set to FALSE for probability plots.
LPR.MEAS$Prob <- interpLine(AA.plt, xfromy=LPR.MEAS$discharge_va, warn=FALSE)
#
# Plot the measurements
AA.plt <- with(LPR.MEAS, addXY(Prob, discharge_va, 
	Plot=list(name="Measured Streamflow", what="points"), current=AA.plt))
# Write the EXPLANATION in the upper-right hand corner
addExplanation(AA.plt, where="ur")
