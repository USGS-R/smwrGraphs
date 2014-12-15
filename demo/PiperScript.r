# This script creates a Piper Diagram for selected cations and anions in a
# data frame. The units can be either mg/L or meq/L. If mg/L, then they can be
# automatically converted. A grouping column can be specified, with colors
# automatically generated or specified in a separate column.
# The required cations are calcium, magnesium and sodium. If potassium is
# selected, then the concentrations are added to sodium and the axis titles are 
# adjusted.
# The required anions are bicarbonate, sulfate, and chloride. If carbonate is
# selected, then the concentrations are added to bicarbonate and the axis titles are 
# adjusted; missing values are set to 0. If fluoride is selected, then the
# concentrations are added to chloride and the axis titles are adjusted; missing 
# values are set to 0. If nitrate plus nitrite is selected, then the
# concentrations are added to chloride and the axis titles are adjusted.
#
# This script is set up to automatically generate a Piper Diagram using default
# symbol size and style. Those and other defaults may be changed in the call to 
# piperPlot and are beyond the 
# End of user input -----------------------
# line.
#
# The following 2 lines required for this script to run, but are not necessary for 
# user-supplied data
library(USGSwsData)
data(IonBalance)
# Begin of user input
# Required for graphs
library(USGSwsGraphs)
# The  USGSwsQW package is automatically loaded if any data are of class "qw"
# Begin user input
# Enter tha name of the dataset
tmp.dsn <- "IonBalance"
# Enter the calcium column
tmp.ca <- "Ca"
# Enter the magnesium column
tmp.mg <- "Mg"
# Enter the sodium column
tmp.na <- "Na"
# Enter the potassium column (optional)
tmp.k <- "K"
# Enter the bicarbonate column
tmp.hco3 <- "Alk"
# Enter the carbonate column (optional)
tmp.co3 <- ""
# Enter the sulfate column
tmp.so4 <- "SO4"
# Enter the chloride column
tmp.cl <- "Cl"
# Enter the fluoride column (optional)
tmp.f <- "F"
# Enter the nitrate plus nitrite column as N (optional)
tmp.no3no2 <- "NO2NO3"
# Convert from mg/L to meq/L
tmp.cvt <- FALSE
# Enter group column, empty string for no group
tmp.grp <- ""
# Enter group column color, "" for automatic color generation
tmp.color <- ""
# Output options
# Set to Valid filename (no .pdf) to create a PDF file, otherwise Screen
tmp.pdf <- "Piper"
# Enter the desired format: page width and height
tmp.width <- 11
tmp.height <- 8.5
#
# End of user input -----------------------
#
tmp.dsn <- get(tmp.dsn)
# Check for class "qw"
tmp.ck <- sapply(tmp.dsn, function(col) inherits(col, "qw"))
if(any(tmp.ck))
	library(USGSwsQW)
# Check on required cations
tmp.ck <- c(tmp.ca, tmp.mg, tmp.na) %in% names(tmp.dsn)
if(!all(tmp.ck))
	stop("not all required cation columns found in the dataset")
# Conversion to numeric requireed if data of class "qw"
tmp.ca <- as.numeric(tmp.dsn[[tmp.ca]])
if(tmp.cvt) 
	tmp.ca <- conc2meq(tmp.ca, "calcium")
tmp.mg <- as.numeric(tmp.dsn[[tmp.mg]])
if(tmp.cvt)
	tmp.mg <- conc2meq(tmp.mg, "magnesium")
tmp.na <- as.numeric(tmp.dsn[[tmp.na]])
if(tmp.cvt)
	tmp.na <- conc2meq(tmp.na, "sodium")
if(tmp.k %in% names(tmp.dsn)) {
	tmp.k <- as.numeric(tmp.dsn[[tmp.k]])
	if(tmp.cvt)
		tmp.k <- conc2meq(tmp.k, "potassium")
	tmp.na <- tmp.na + tmp.k
	tmp.zcat <- "Sodium plus Potassium"
} else
	tmp.zcat <- "Sodium"
#
tmp.ck <- c(tmp.hco3, tmp.so4, tmp.cl) %in% names(tmp.dsn)
if(!all(tmp.ck))
	stop("not all required anion columns found in the dataset")
tmp.hco3 <- as.numeric(tmp.dsn[[tmp.hco3]])
if(tmp.cvt)
	tmp.hco3 <- conc2meq(tmp.hco3, "bicarbonate")
tmp.so4 <- as.numeric(tmp.dsn[[tmp.so4]])
if(tmp.cvt)
	tmp.so4 <- conc2meq(tmp.so4, "sulfate")
tmp.cl <- as.numeric(tmp.dsn[[tmp.cl]])
if(tmp.cvt)
	tmp.cl <- conc2meq(tmp.cl, "chloride")
# Add extra anions
if(tmp.co3 %in% names(tmp.dsn)) {
	tmp.co3 <- as.numeric(tmp.dsn[[tmp.co3]])
	tmp.co3 <- na2miss(tmp.co3, 0)
	if(tmp.cvt)
		tmp.co3 <- conc2meq(tmp.co3, "carbonate")
	tmp.hco3 <- tmp.hco3 + tmp.co3
	tmp.yan <- "Carbonate plus Bicarbonate"
} else
	tmp.yan <- "Bicarbonate"
# check fluoride
if(tmp.f %in% names(tmp.dsn)) {
	tmp.f <- as.numeric(tmp.dsn[[tmp.f]])
	tmp.f <- na2miss(tmp.f, 0)
	if(tmp.cvt)
		tmp.f <- conc2meq(tmp.f, "fluoride")
	tmp.cl <- tmp.cl + tmp.f
	tmp.xan <- "Chloride plus Fluoride"
} else
	tmp.xan <- "Chloride"
# check no2+no3
if(tmp.no3no2 %in% names(tmp.dsn)) {
	tmp.no3no2 <- as.numeric(tmp.dsn[[tmp.no3no2]])
	if(tmp.cvt)
		tmp.no3no2 <- conc2meq(tmp.no3no2, "nitrate")
	tmp.cl <- tmp.cl + tmp.no3no2
	if(tmp.xan == "Chloride plus Fluoride") {
		tmp.xan <- "Chloride, Fluoride, Nitrate plus Nitrite"
	} else
		tmp.xan <- "Chloride, Nitrate plus Nitrite"
} # No need to modify tmp.xan as set for no no3+no2
#
# Ready, set up and create the graphs
if(tmp.pdf == "") {
	setPage(list(width=tmp.width, height=tmp.height), font="USGS", device="windows")
} else
	setPDF(list(width=tmp.width, height=tmp.height), basename=tmp.pdf)
if(tmp.grp == "") {
piperPlot(xCat=tmp.ca,
					yCat=tmp.mg,
					zCat=tmp.na,
					xAn=tmp.cl,
					yAn=tmp.hco3,
					zAn=tmp.so4,
					zCat.title=tmp.zcat,
					xAn.title=tmp.xan,
					yAn.title=tmp.yan)
} else {
	tmp.grp <- as.character(tmp.dsn[[tmp.grp]])
	if(tmp.color == "") {
		tmp.color <- setColor(tmp.grp)
	} else
		tmp.color <- tmp.dsn[[tmp.color]]
	## Set graph layout
	# Set explanation from documentation for setLayout
	tmp.expl <- max(nchar(tmp.grp)) / 17 + 0.5
	#tmp.expl <- 2.5
	tmp.size <- min(tmp.height - 1, tmp.width - 1 - tmp.expl)
  tmp.lo <- setLayout(width=tmp.size, height=tmp.size, 
  										explanation=list(right=tmp.expl))
	setGraph(1, tmp.lo)
	tmp.pl <- piperPlot(xCat=tmp.ca,
											yCat=tmp.mg,
											zCat=tmp.na,
											xAn=tmp.cl,
											yAn=tmp.hco3,
											zAn=tmp.so4,
											Plot=list(name=tmp.grp, color=tmp.color),
											zCat.title=tmp.zcat,
											xAn.title=tmp.xan,
											yAn.title=tmp.yan)
	setGraph("explanation", tmp.lo)
	addExplanation(tmp.pl)
}
# Close pdf if necesssary
if(tmp.pdf != "")
	dev.off()
# Clean up, these are always created
rm(tmp.dsn, tmp.ca, tmp.mg, tmp.na, tmp.k, tmp.hco3, tmp.co3, tmp.so4, tmp.cl, 
	 tmp.no3no2, tmp.f, tmp.cvt, tmp.grp, tmp.color, tmp.width, tmp.height, 
	 tmp.pdf, tmp.ck, tmp.yan, tmp.xan, tmp.zcat)
# Created if explanation
if(exists("tmp.expl"))
	rm(tmp.expl, tmp.size, tmp.lo, tmp.pl)
