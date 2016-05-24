#' install package from local drive
#'
#' Basically just an enhanced shortcut to \code{devtools::\link[devtools]{install}}
#'
#' @details Removes function objects from workspace and (toDo!) unloads reverse dependencies first.
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2014
#' @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords package
#' @importFrom devtools install
#' @export
#' @examples
#' # installB()
#'
#' @param package Package name. DEFAULT: "berryFunctions"
#' @param path Path containing package folder. DEFAULT: "S:/Dropbox/Public"
#'
installB <- function(
package="berryFunctions",
path="S:/Dropbox/Public"
)
{
# laptop linux path change:
if(!file.exists(path)) path <- gsub("S:", "~", path)
# work PC path change:
if(!file.exists(path)) path <- gsub("~", "C:/Users/boessenkool", path)
#
# Somehow this section changes "berryFunctions" into "package" if called from updatePackage()
# package <- deparse(substitute(package))
# package <- gsub("\"", "", package, fixed=TRUE)
#
# remove function objects from workspace
d <- dir(paste0(path, "/", package, "/R"))
d <- gsub(".r", "", d, fixed=TRUE)
d <- gsub(".R", "", d, fixed=TRUE)
l <- ls(globalenv())
rm(list=l[l %in% d], envir=globalenv())
# unload package and dependencies to avoid unloadNamespace * not successful. Forcing unload." messages
try(unloadNamespace("extremeStat"), silent=TRUE)
# install
devtools::install(paste0(path, "/", package))
require(package, character.only=TRUE, quietly=TRUE)
}
