# Hourly or daily precipitation data can be retrieved for many sites from
# the NOAA climate website: http://www.nws.noaa.gov/climate/
# These data were retrieved for Thief River Falls, Minn. on November 14, 2012
# Note that Crookston would be closer but missing record for this time period
TRFPrecip <- data.frame(DateTime=
structure(c(1191823200, 1191909600, 1192773600, 1191826800, 1191913200, 
1192777200, 1191830400, 1191916800, 1192780800, 1191747600, 1191920400, 
1192784400, 1191837600, 1191924000, 1192788000, 1191841200, 1191927600, 
1192705200, 1192791600, 1191844800, 1191931200, 1192708800, 1192795200, 
1191848400, 1192712400, 1191852000, 1192716000, 1191855600, 1192719600, 
1191859200, 1192723200, 1191862800, 1192726800, 1191866400, 1192730400, 
1191351600, 1192734000, 1192820400, 1191873600, 1192737600, 1191618000, 
1192741200, 1192744800, 1191625200, 1191884400, 1192748400, 1191888000, 
1192752000, 1191891600, 1192755600, 1191895200, 1192759200, 1191898800, 
1192762800, 1191902400, 1192680000, 1192766400, 1191819600, 1191906000, 
1192597200, 1192683600, 1192770000), class = c("POSIXct", "POSIXt"
), tzone = "America/Chicago"), Precip=
c(0.02, 0.06, 0.09, 0.01, 0.07, 0.06, 0.01, 0.01, 0.03, 0.01, 
0.01, 0.01, 0.12, 0.04, 0.01, 0.06, 0.02, 0.03, 0.01, 0.03, 0.02, 
0.05, 0.02, 0.07, 0.06, 0.16, 0.05, 0.08, 0.04, 0.12, 0.07, 0.08, 
0.05, 0.03, 0.08, 0.04, 0.08, 0.01, 0.01, 0.06, 0.05, 0.04, 0.02, 
0.01, 0.03, 0.02, 0.03, 0.05, 0.02, 0.06, 0.06, 0.05, 0.04, 0.05, 
0.1, 0.04, 0.08, 0.01, 0.02, 0.01, 0.02, 0.09))
library(USGSwsGraphs)
library(USGSwsData)
data(GlacialRidge)
# Set up the page
setGD("GlacialRidge")
# Set the layout, share x-axes
AA <- setLayout(width=6, height=c(1.5, 3.5), shared.x=0.75)
# Graph 1 has not tick on the bottom
AA.sg <- setGraph(1, AA, noTicks=1)
with(TRFPrecip, timePlot(DateTime, Precip, Plot=list(what="vert"),
  xaxis.range=as.Date(c("2007-10-01", "2007-10-31")),
  margin=AA.sg,
  yaxis.rev=T, xtitle="", ytitle="Precipitation, inches"))
# Graph 2 has not tick on the top
AA.sg <- setGraph(2, AA, noTicks=3)
with(subset(GlacialRidge, datetime < as.Date("2007-11-01")), 
  timePlot(datetime, G01, margin=AA.sg, yaxis.rev=T,
  xtitle="", ytitle="Water Level, in feet below land surface"))

