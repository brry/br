#' @title check for code{} and link{} statements
#' @description  check for code{} and link{} statements where the leading slash 
#' has been forgotten in the source code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @param path Package path, Character string. DEFAULT: "." (current getwd)
checkCodeLink <- function(path=".")
{
path <- paste0(berryFunctions::packagePath(path),"/R")
rf <- dir(path, full.names=TRUE)
errors <- sapply(rf, function(f)
{
rl <- readLines(f)
err <- c(grep("(?<!/)code{", gsub("\\\\", "/", rl), perl=TRUE),
         grep("(?<!/)link{", gsub("\\\\", "/", rl), perl=TRUE)  )
err <- unique(err)
paste(err, rl[err], sep=": ")
}, simplify=FALSE)
errors <- errors[sapply(errors, length)>0]
if(length(errors)<1) 
  {
  message("No missing leading slashes found in ", path)
  return(invisible())
  }
errors <- lapply(errors, paste, collapse="\n")
errors <- paste(names(errors), unlist(errors), sep="\n")
errors <- paste(errors, collapse="\n\n")
message("missing leading slashes found in ", path, ":\n\n", errors)
}
