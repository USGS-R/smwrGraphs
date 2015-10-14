# Create an index to color ramp ranges
library(smwrGraphs)
# Set up the page, for this demo, use the default layout
setGD("Colors")
# This demo script sets up greenRed, blueRed, warmCool and the pastel
# color ramps. The ramps redGreen, redBle, and coolWarm are simply 
# the reverse order. The pastel ramp chooses widely separated colors
# that should be relatively easy to distingush from each other.
##
# Set x to repeating sequence of 1 to 5 for the 5 colors
x <- rep(1:5, 4) -.5
# Set y to repeating sequence for each row
y <- as.numeric(rep(1:4, each = 5))
# set colors along each row
z <- c(greenRed.colors(5), blueRed.colors(5), warmCool.colors(5),
			 pastel.colors(5))
# Set the margin for y-axis labels to be added later in margin argument
colorPlot(x, y, z, Plot=list(symbol="square", color="Index", size=.4),
       ytitle="", xtitle="", xlabels=5, margin=c(NA, -6, NA, NA))
# Now add ramp labels
addLabel("greenRed", 1, side="left", justification="right",
       orientation="perpendicular")
addLabel("blueRed", 2, side="left", justification="right",
				 orientation="perpendicular")
addLabel("warmCool", 3, side="left", justification="right",
				 orientation="perpendicular")
addLabel("pastel", 4, side="left", justification="right",
				 orientation="perpendicular")
