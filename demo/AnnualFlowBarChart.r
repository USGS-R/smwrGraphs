# Create a bar chart of annual flows
library(smwrGraphs)
library(smwrData)
data(ConecuhFlows)
# Set up the page, for this demo, use the default layout
setGD("Conecuh")
# Set up x margin to suppress ticks and labels. the selected value
# for the first element (bottom margin) will work for 4-digit years
AA.margin <- c(-3.5, NA, -0.5, NA)
# The default x-axis range would be from 1940 - 1965.
# Set the x-axis range so that each year can be labeled and maximize the 
#  plotted area.
AA.xr <- range(ConecuhFlows$Year) + c(-1, 1)
# The y-axis must include 0, the pretty function will select a suitable
#  maximim value.
AA.yr <- c(0, max(pretty(ConecuhFlows$Flow)))
# Suppress plotting anything
AA.pl <- with(ConecuhFlows, timePlot(Year, Flow, Plot=list(what="none"),
  yaxis.range=AA.yr, xaxis.range=AA.xr, xtitle="Water Year",
  ytitle="Mean Annual Streamflow, in cubic feet per second",
  margin=AA.margin))
# Add the bars
with(ConecuhFlows, addBars(Year, Flow, Bars=list(width=1), current=AA.pl))
# And label each year
for(AA.lb in ConecuhFlows$Year) 
  addLabel(AA.lb, AA.lb, orientation="perp", justification="right")
# Finish with figure caption
addCaption("Mean annual streamflow for the Conecuh River at Brantley, AL")