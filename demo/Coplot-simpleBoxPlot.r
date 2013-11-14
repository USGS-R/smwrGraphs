# Create a bar chart of annual flows
library(USGSwsGraphs)
library(USGSwsData)
data(MiningIron)
# Set up the page, for this demo, use the default layout
setGD("MiningIron")
# No explanation for this example
# yleft must be set because of the range of the Iron values
condition(boxPlot(Iron, group=MineType, yaxis.log=TRUE,
                  yaxis.range=c(0.01, 1000),
                  xlabels.rotate=TRUE, margin=.margin),
          data=MiningIron, group="Rock", num.cols=2,
          yleft=4.2, ytitle="Iron concentration, in mg/L")
