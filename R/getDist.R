# Get a function for a distribution
# Support for probability plotting functions in USGS library
# Not all probability distributions are supported
#  Sampling distributions, such as F and students, discrete 
#  distributions and a few unusual ones, such as cauchy, are 
#  not supported. Also, they must have default parameters.
#
# Coding History:
#    2008May03 DLLorenz Original coding.
#    2008Jun10 DLLorenz aded substitute function for qunif-- qunif returns -Inf
#                       for p=0
#    2010Nov29 DLLorenz Begin modifications for R
#    2010Nov29          This version.
#

getDist.fcn <- function(distribution, what='q') {
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
