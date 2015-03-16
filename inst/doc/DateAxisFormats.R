### R code from vignette source 'DateAxisFormats.Rnw'

###################################################
### code chunk number 1: DateAxisFormats.Rnw:23-28
###################################################
# Load the smwrGraphs package
library(smwrGraphs)
# Generate the data to plot
DD <- as.POSIXct(c("2009-07-02 10:30", "2009-07-02 12:30"))
YY <- c(0.14, 0.82)


###################################################
### code chunk number 2: DateAxisFormats.Rnw:38-47
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph01", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="")
# add 15-minute minor ticks (3 internal per hour)
addMinorTicks("bottom", AA.pl, 3)
addMinorTicks("top", AA.pl, 3)
graphics.off()


###################################################
### code chunk number 3: DateAxisFormats.Rnw:57-67
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph02", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-02", "2009-07-03")))
# add hourly minor ticks (5 internal per hour)
addMinorTicks("bottom", AA.pl, 5)
addMinorTicks("top", AA.pl, 5)
graphics.off()


###################################################
### code chunk number 4: DateAxisFormats.Rnw:82-93
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph03", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-02", "2009-07-03")),
                  xlabels="days")
# add hourly minor ticks (5 internal per hour)
addMinorTicks("bottom", AA.pl, 23)
addMinorTicks("top", AA.pl, 23)
graphics.off()


###################################################
### code chunk number 5: DateAxisFormats.Rnw:101-112
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph04", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-02", "2009-07-03")),
                  xlabels=list(major="days", style="between"))
# add hourly minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)
graphics.off()


###################################################
### code chunk number 6: DateAxisFormats.Rnw:122-129
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph05", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-01", "2009-07-05")))
graphics.off()


###################################################
### code chunk number 7: DateAxisFormats.Rnw:137-144
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph06", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-28", "2009-07-05")))
graphics.off()


###################################################
### code chunk number 8: DateAxisFormats.Rnw:152-159
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph07", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-20", "2009-07-18")))
graphics.off()


###################################################
### code chunk number 9: DateAxisFormats.Rnw:174-184
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph08", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-01", "2009-08-01")))
# add daily minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)
graphics.off()


###################################################
### code chunk number 10: DateAxisFormats.Rnw:192-199
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph09", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-01", "2010-06-01")))
graphics.off()


###################################################
### code chunk number 11: DateAxisFormats.Rnw:207-215
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph10", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-01-01", "2010-06-01")))

graphics.off()


###################################################
### code chunk number 12: DateAxisFormats.Rnw:230-241
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph11", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-01-01", "2011-01-01")),
                  xlabels="year")
# add monthly minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)
graphics.off()


###################################################
### code chunk number 13: DateAxisFormats.Rnw:249-259
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph12", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2000-01-01", "2013-06-01")))
# add intervening year minor ticks
addMinorTicks("bottom", AA.pl, 1)
addMinorTicks("top", AA.pl, 1)
graphics.off()


###################################################
### code chunk number 14: DateAxisFormats.Rnw:274-285
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph13", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2008-10-01", "2009-10-01")),
                  xlabels="water year")
# add intervening year minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)
graphics.off()


###################################################
### code chunk number 15: DateAxisFormats.Rnw:293-301
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setSweave("graph14", 6, 1.5)
# 
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2005-10-01", "2010-10-01")),
                  xlabels="water year")
graphics.off()


