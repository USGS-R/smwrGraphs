# Add grid lines
#
# Coding history:
#    2012Nov11 DLLorenz Original Coding
#    2012Nov12          This version.
#

addGrid <- function(current, Xgrid=list(grid="gray50", finegrid="none"),
                    Ygrid=list(grid="gray50", finegrid="none")) {
  Xgrid <- setDefaults(Xgrid, grid="gray50", finegrid="none")
  Ygrid <- setDefaults(Ygrid, grid="gray50", finegrid="none")
  if(!is.null(current$xax)) {
    Not <- current$xax$range
    if(Xgrid$grid != "none") {
      Grd <- current$xax$ticks
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(v=Grd, col=Xgrid$grid, lwd=frameWt())
    }
    if(Xgrid$finegrid != "none") {
      Grd <- current$xax$finegrid
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(v=Grd, col=Xgrid$finegrid, lwd=frameWt())
    }
  }
  if(!is.null(current$yax)) {
    Not <- current$yax$range
    if(Ygrid$grid != "none") {
      Grd <- current$yax$ticks
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(h=Grd, col=Ygrid$grid, lwd=frameWt())
    }
    if(Ygrid$finegrid != "none") {
      Grd <- current$yax$finegrid
      Grd <- Grd[!(Grd %in% Not)]
      Not <- c(Not, Grd)
      abline(h=Grd, col=Ygrid$finegrid, lwd=frameWt())
    }
  }
  invisible()
}
