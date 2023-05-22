#' @title Is a package installed and usable?
#' @description Is a package installed and usable?
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2017
#' 
#' @param package Package character string passed to  \code{\link{requireNamespace}}
#' 
isInstalled <- function(package)  
{
suppressMessages(suppressWarnings(
{out <- requireNamespace(package, quietly=TRUE)
}))
out
}
