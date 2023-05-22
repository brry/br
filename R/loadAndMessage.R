#' @export
#' @rdname installB
#' @param message Write custom message in loadAndMessage? DEFAULT: TRUE
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
}