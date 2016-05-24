#' install package from local drive
#'
#' load package and give verbose message
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{loadPackages}}
#' @keywords package
#' @export
#'
#' @param package Package name
#' @param quiet Logical: suppress messages like "package was built under R version xyz"
#'
loadAndMessage <- function(package, quiet=TRUE)
  {
  if(quiet) suppressWarnings( 
            require(package, character.only=TRUE, quietly=TRUE))  else
  require(package, character.only=TRUE, quietly=TRUE)
  # prepare message
  version <- utils::packageDescription(package)$Version
  date <- utils::packageDescription(package)$Date
  message("Loaded package ", format(package,width=15), "Version ",
          format(version,width=7), " from ", date)
}
