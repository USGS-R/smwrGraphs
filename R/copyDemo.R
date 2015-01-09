#' Copy a Demo File
#' 
#' Copies a demo file from the source package to a file.
#' 
#' 
#' @param topic the name of the topic, must be a character string.
#' @param package the package name, must be a character string.
#' @param file the target file name, must be a character string. If missing,
#' then the file name is created from the \code{topic} name.
#' @return Either the name of the target file or \code{NULL} if the copy
#' failed.
#' @seealso \code{\link{demo}}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{copyDemo("HydroPrecip")}
#' 
#' @export copyDemo
copyDemo <- function(topic, package="smwrGraphs",
                     file) {
	# Coding history:
	#    2012Nov16 DLLorenz Initial coding nad begin edits
	#    2014Jun25 DLLorenz Converted to roxygen
	#
  topic <- setFileType(topic, "R", TRUE)
  if(missing(file))
    file <- topic
  ## From help(demo):
  in.file <- system.file("demo", topic, package=package)
  if(file.copy(in.file, file)) # Will not overwrite an existing file!
    return(file)
  else
    return()
}
