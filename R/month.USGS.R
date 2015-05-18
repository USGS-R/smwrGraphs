#' Month Abbreviations
#' 
#' A vector of USGS-style month abbreviations.
#' 
#' @format A named character vector of the 12 preferred forms for month abbreviations.
#' @references Hansen, W.R., 1991, Suggestions to Authors of the United States Geological Survey, 
#'7th Ed.: U.S. Govenement Printing Office, 289 p.
#' @examples
#' print(month.USGS)
#' \dontrun{
#' # For examples of month.USGS in graphs see
#' vignette(topic="LineScatter", package="smwrGraphs")
#' }
#' @export
month.USGS <- c(January="Jan.", February="Feb.", March="Mar.", 
								April="Apr.", May="May", June="June", July="July", 
								August="Aug.", September="Sept.", October="Oct.", 
								November="Nov.", December="Dec.")
