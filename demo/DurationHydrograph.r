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
# Set up the page, for this demo, set up room for explanation at bottom
setGD("Chpptank")
AA.lo <- setLayout(explanation=list(bottom=1.5))
setGraph(1, AA.lo)
# Create the empty graph
AA.pl <- with(ChoptankFlow, timePlot(datetime, Flow, Plot=list(what="none"),
    ytitle="Streamflow, in cubic feet per second",
    yaxis.log=TRUE,
    xaxis.range=as.Date(c("2009-10-01", "2010-09-30"))))
# Create a sequence of 5 areas shaded quantiles of flow
Colors <- warmCool.colors(5)
# These are used for the explanation, not needed if no explanation is needed
names(Colors) <- c("0 to 10 percentile", "10 to 25 percentile", 
    "25 to 75 percentile", "75 to 90 percentile", "90 to 100 percentile")
# Modify the duration matrix to match the desired date range of the output
# For these data, Feb 29, 2010 does not exist, so remove it
Choptank.dur <- Choptank.dur[-152L,]
# Generate data sequence
Choptank.dates <- seq(as.Date("2009-10-01"), as.Date("2010-09-30"), by="days")
for(i in seq(5))
    AA.pl <- addArea(Choptank.dates,
        Choptank.dur[, i+1], Choptank.dur[, i],
        Area=list(name=names(Colors)[i],
            color=Colors[i], outline="none"),
        current=AA.pl)

# Note the offest of 0.7485 must be added to the x values to adjust for the
# water-year offset. a value of 0.248 would need to be added for climate-year
# data. These are the offsets in the transformation function for the x-axis data

# Now add the hydrograph for water-year 2010
AA.pl <- with(subset(ChoptankFlow, datetime >= "2009-10-01" & datetime <= "2010-09-30"),
    addXY(datetime, Flow, Plot=list(name="Streamflow for water year 2010"),current=AA.pl))
# Add the explanation
setGraph("explanation", AA.lo)
addExplanation(AA.pl)