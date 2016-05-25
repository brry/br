#' install package from local drive
#'
#' Just a shortcut to \code{\link{installB}} with the default \code{package="extremeStat"}
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2015
#' @seealso \code{\link{installB}}, \code{\link{loadPackages}}
#' @keywords package
#' @export
#'
#' @param path Path containing package folder. DEFAULT: "S:/Dropbox/Public"
#'
installE <- function(
path="S:/Dropbox/Public") # path containing package folder
{
installB(package="extremeStat", path=path)
}
