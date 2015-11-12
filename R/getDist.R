#' Distribution Function
#' 
#' A function to compute the density, cumulative distribution, quantile, or
#' random generation of the specified function. This function is used primarily
#' as a support function for \code{probPlot}.
#' 
#' For general use, \code{distribution} should be one of "normal," "lognormal,"
#' "pearsonType3," "logpearsonType3," "exponential," "logistic," or "uniform."
#' Partial matching is done, so only as many characters to make a unique match
#' are required. Other distributions can be retrieved by specifying the base
#' name of the distribution. That option can be useful if other packages that
#' contain distribution functions have been loaded.
#' 
#' @param distribution the name of the distribution. See \bold{Details}.
#' @param what a character indicating which form to retrun. Must be "q" for
#' quantile, "d" for density, "p" for cumulative distribution, or "r" for
#' random generation.
#' @return The specified function.
#' @seealso 
#Flip for production/manual
#'\code{\link[stats]{Normal}}, \code{\link[stats]{Lognormal}},
#'\code{\link[stats]{Exponential}}, \code{\link[stats]{Logistic}}, 
#'\code{\link[stats]{Uniform}},
#\code{Normal}, \code{Lognormal}, \code{Exponential}, 
#\code{Logistic}, \code{Uniform} (all in stats package),
#' \code{\link{PearsonIII}}, \code{\link{LogPearsonIII}},
#' @keywords dplot
#' @export getDist.fcn
getDist.fcn <- function(distribution, what='q') {
	# Coding History:
	#    2008May03 DLLorenz Original coding.
	#    2008Jun10 DLLorenz aded substitute function for qunif-- qunif returns -Inf
	#                       for p=0
	#    2010Nov29 DLLorenz Begin modifications for R
	#    2014Jun26 DLLorenz Converted to roxygen
  match.list <- c("normal", "lognormal", "pearsonType3", 
                  "logpearsonType3", "exponential", "logistic", "uniform")
  match.base <- c("norm", "lnorm", "pearsonIII", "lpearsonIII", 
                  "exp", "logis", "unif")
  match.fcn <- pmatch(distribution, match.list)
  if(is.na(match.fcn)) {
    ## Try getting the function from some other loaded package
    fcn.name <- paste(substring(what, 1,1), distribution, sep='')
    if(exists(fcn.name))
       return(get(fcn.name))
    stop("Distribution function for ", distribution, " not found.")
  }
  if(match.fcn == 7 && what == 'q') # uniform
    return(function(p, min = 0., max = 1.) 
           return(min + p * (max - min)))
  ## otherwise
  return(get(paste(substring(what, 1,1), match.base[match.fcn], sep='')))
}
