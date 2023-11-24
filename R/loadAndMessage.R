#' @title Call \code{\link{require}} and give verbose output
#' @param package Package name
#' @param quiet suppress messages like "package was built under R version xyz". DEFAULT: TRUE
#' @param message Write custom message? DEFAULT: TRUE
loadAndMessage <- function(package, quiet=TRUE, message=TRUE)
{
if(quiet) suppressMessages(suppressWarnings(
              require(package, character.only=TRUE, quietly=TRUE)))  else
              require(package, character.only=TRUE, quietly=TRUE)
# prepare message
if(!message) return(invisible(NULL))
packdesc <- utils::packageDescription(package)
if(all(is.na(packdesc))) return(paste0("loading package '", package, "' failed."))
version <- packdesc$Version
date    <- packdesc$Date
message("Loaded package ", format(package,width=15), "Version ",
          format(version,width=7), " from ", date)
checkOutdated(package)
}
