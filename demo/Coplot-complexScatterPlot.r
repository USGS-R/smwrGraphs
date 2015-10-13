# Create a conditioned graph
library(smwrGraphs)
library(smwrData)
data(UraniumTDS)
# Set up the page, for this demo, use the default layout
setGD("UraniumTDS")
# The explanation is automatically set up after the last graph
# but only displayed on the last page if multiple pages.
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
