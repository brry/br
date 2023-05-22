#' @title open Rprofile.site
#' @description open Rprofile.site in default viewer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2019
#' @export
profilesite <- function() 
{
path <- paste0( R.home("etc"), "/Rprofile.site")
berryFunctions::openFile(path)
}
