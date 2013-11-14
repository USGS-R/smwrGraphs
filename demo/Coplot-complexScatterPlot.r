# Create a bar chart of annual flows
library(USGSwsGraphs)
library(USGSwsData)
data(UraniumTDS)
# Set up the page, for this demo, use the default layout
setGD("UraniumTDS")
# No explanation for this example
# yleft must be set because of the range of the Iron values
condition({AA.ret <- xyPlot(TDS, Uranium, Plot=list(name="Sample data"),
                            xaxis.range=c(0, 1500),
                            yaxis.range=c(0, 16), 
                            xtitle="", ytitle="", margin=.margin);
           AA.ret <- refLine(coef=coef(lm(Uranium ~ TDS)),
                             Plot=list(name="Regression line"),
                             current=AA.ret)
           AA.ret},
          data=UraniumTDS, group="HCO3", num.cols=2,
          group.name="Bicarbonate ",
          explanation=list(bottom=1.5),
          ytitle="Uranium concentration, in ug/L",
          xtitle="Total dissolved solids, in mg/L") -> AA.pl
addExplanation(AA.pl)
