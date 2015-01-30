# Create a duration hydrograph for water year 2010
# No explanation created for this graph
library(smwrGraphs)
library(smwrData)
data(ChoptankFlow)

# Construct the daily duration statistics by water year The final output
#is a matrix that contains the 6 columns of the selected quantiles.
Choptank.dur <- with(ChoptankFlow, tapply(Flow, baseDay(datetime, FALSE, year="water"), 
																					FUN=quantile,
																					probs=c(0,.1,.25,.75,.9, 1)))
Choptank.dur <- do.call(rbind, Choptank.dur)
# Set up the page, for this demo, use the default layout
setGD("Chpptank")
# Create the empty graph
AA.pl <- with(ChoptankFlow, timePlot(datetime, Flow, Plot=list(what="none"),
																		 ytitle="Streamflow, in cubic feet per second",
																		 yaxis.log=TRUE,
																		 xaxis.range=as.Date(c("2009-10-01", "2010-09-30"))))
# Create a sequence of 5 areas shaded quantiles of flow
Colors <- warmCool.colors(5)
# Modify the duration matrix to match the desired date range of the output
# For these data, Feb 29, 2010 does not exist, so remove it
Choptank.dur <- Choptank.dur[-152L,]
# Generate data sequence
Choptank.dates <- seq(as.Date("2009-10-01"), as.Date("2010-09-30"), by="days")
for(i in seq(5))
	addArea(Choptank.dates,
					Choptank.dur[, i+1], Choptank.dur[, i],
					Area=list(color=Colors[i], outline="none"),
					current=AA.pl)

# Note the offest of 0.7485 must be added to the x values to adjust for the
# water0year offset. a value of 0.248 would need to be added for cliamte-year
# data. These are the offsets in the transformation function for the x-axis data

# Now add the hydrograph for water-year 2010
with(subset(ChoptankFlow, datetime >= "2009-10-01" & datetime <= "2010-09-30"),
		 addXY(datetime, Flow, current=AA.pl))

