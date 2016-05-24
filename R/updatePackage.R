#' install package from local drive
#'
#' install current development version of package from folder
#'
#' @details Calls \code{\link{installB}} if the package date or version in the source code has been changed.
#'
#' @return Nothing
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2014
#' @seealso \code{\link{installB}}, \code{\link{packageDescription}}, \code{\link{read.dcf}}
#' @keywords package
#' @export
#' @examples
#' # See ?loadPackages
#'
#' @param package Package name. DEFAULT: "berryFunctions"
#' @param path Path containing package folder. DEFAULT: "S:/Dropbox/Public"
#'
updatePackage <- function(
package="berryFunctions",
path="S:/Dropbox/Public"
)
if(interactive())
{
if(!requireNamespace(package, quietly=TRUE))
  stop("package ", package, " must be installed before updatePackage can work.")
# remove end slash
while(substring(path, nchar(path))=="/")
   path <- substring(path, 1, nchar(path)-1)
# laptop linux path change:
if(!file.exists(path)) path <- gsub("S:", "~", path)
# work PC path change:
if(!file.exists(path)) path <- gsub("~", "C:/Users/boessenkool", path)
# path control
if(!file.exists(path)) stop("path does not exist. ", path)
# installed date/version:
Vinst <- utils::packageDescription(package)[c("Date","Version")]
# date in source code
Vsrc <- read.dcf(file=paste0(path,"/",package, "/DESCRIPTION"), fields=c("Date","Version"))
# install if outdated:
if( as.Date(Vsrc[,"Date"]) > as.Date(Vinst$Date) | Vsrc[,"Version"] > Vinst$Version)
  installB(package, path)
# load:
loadAndMessage(package)
}
