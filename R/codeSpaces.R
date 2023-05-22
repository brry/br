#' @title Correct spaces in source code
#' @description Remove trailing spaces in source code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @param path Package path, Character string. DEFAULT: "." (current getwd)
codeSpaces <- function(path=".")
{
path <- paste0(berryFunctions::packagePath(path),"/R")
message("Correcting trailing spaces in files at ", path)
owd <- setwd(path) ; on.exit(setwd(owd))
remSpace <- function(file)
  {
  d <- d_orig <- readLines(file, warn=FALSE)
  trim <- function(x) {while(endsWith(x," ")) x <- substring(x, 1, nchar(x)-1); x}
  d <- sapply(d, trim, USE.NAMES=FALSE)
  d[d=="#'"] <- "#' "
  writeLines(d, file)
  sum(d_orig != d)
  }
nchanges <- sapply(dir(), remSpace)
if(any(nchanges!=0)) message("Number of lines changed:")
return(nchanges[nchanges!=0])
}
