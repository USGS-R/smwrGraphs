## ----setup, echo=FALSE, warning=FALSE, message=FALSE---------------------
library(knitr)
library(rmarkdown)
library(captioner)
opts_chunk$set(message=FALSE,warning=FALSE,dev="png")
fig_nums <- captioner()

## ------------------------------------------------------------------------
# Load the smwrGraphs package
library(smwrGraphs)
# Generate the data to plot
DD <- as.POSIXct(c("2009-07-02 10:30", "2009-07-02 12:30"))
YY <- c(0.14, 0.82)

## ----firstChunk----------------------------------------------------------
# Set up the graphics environment, the equivalent call for an on screen
#  device could be setPage("square")
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="")
# add 15-minute minor ticks (3 internal per hour)
addMinorTicks("bottom", AA.pl, 3)
addMinorTicks("top", AA.pl, 3)


## ----secondChunk---------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-02", "2009-07-03")))
# add hourly minor ticks (5 internal per hour)
addMinorTicks("bottom", AA.pl, 5)
addMinorTicks("top", AA.pl, 5)


## ----thirdChunk----------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-02", "2009-07-03")),
                  xlabels="days")
# add hourly minor ticks (5 internal per hour)
addMinorTicks("bottom", AA.pl, 23)
addMinorTicks("top", AA.pl, 23)


## ----fourthChunk---------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-02", "2009-07-03")),
                  xlabels=list(major="days", style="between"))
# add hourly minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)


## ----fifthChunk----------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-07-01", "2009-07-05")))


## ----sixthChunk----------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-28", "2009-07-05")))


## ----seventhChunk--------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-20", "2009-07-18")))


## ----eighthChunk---------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-01", "2009-08-01")))
# add daily minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)


## ----ninthChunk----------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-06-01", "2010-06-01")))


## ----tenthChunk----------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-01-01", "2010-06-01")))


## ----eleventhChunk-------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2009-01-01", "2011-01-01")),
                  xlabels="year")
# add monthly minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)

## ----twelthChunk---------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2000-01-01", "2013-06-01")))
# add intervening year minor ticks
addMinorTicks("bottom", AA.pl, 1)
addMinorTicks("top", AA.pl, 1)

## ----thirteenChunk-------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2008-10-01", "2009-10-01")),
                  xlabels="water year")
# add intervening year minor ticks
addMinorTicks("bottom", AA.pl)
addMinorTicks("top", AA.pl)


## ----fourteenChunk-------------------------------------------------------
setPNG()
AA.pl <- timePlot(DD, YY, Plot=list(what="none"), ylabels=c(0,1), ytitle="",
                  xaxis.range=as.Date(c("2005-10-01", "2010-10-01")),
                  xlabels="water year")


