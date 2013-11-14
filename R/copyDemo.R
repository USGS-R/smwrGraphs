# Copy a demo
#
# Coding history:
#    2012Nov16 DLLorenz Initial coding nad begin edits
#    2012Nov16          This version.
#

copyDemo <- function(topic, package="USGSwsGraphs",
                     file) {
  ## Args:
  ##  topic, character, the name of the demo topic
  ##  package, cahracter, the name opf the package
  ##  file, character, the name of the output file. If mising,
  ##   then create name fro m topic
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
