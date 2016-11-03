#' create and open a file with the content of all the functions in all my packages
#'
#' @return file path
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Okt 2016
#' @export
#' @examples
#' # allFunctions()
#'
#' @param \dots Arguments passed to \code{combineFiles} like quiet=TRUE
#'
allFunctions <- function(
...
)
{
  owd <- getwd()
  on.exit(setwd(owd))
  if(Sys.info()["user"]=="berry")       setwd(                  'S:/Dropbox/Public')
  if(Sys.info()["sysname"]=="Linux")    setwd(         "/home/berry/Dropbox/Public")
  #if(Sys.info()["user"]=="boessenkool")setwd(                  'D:/Dropbox/Public')
  if(Sys.info()["nodename"]=="GK-PC-2") setwd('C:/Users/boessenkool/Dropbox/Public')

  d <-      dir("berryFunctions/R", full.names=TRUE)
  d <- c(d, dir("extremeStat/R",    full.names=TRUE))
  d <- c(d, dir("OSMscale/R",       full.names=TRUE))
  d <- c(d, dir("rdwd/R",           full.names=TRUE))
  d <- c(d, dir("process_of_creating_package/installB/R",         full.names=TRUE))
  d <- c(d, dir("process_of_creating_package/shapeInteractive/R", full.names=TRUE))

  outFile <- paste0("process_of_creating_package/__All_functions_", Sys.Date(), ".r")

  berryFunctions::combineFiles(inFiles=d, outFile=outFile, ...)
  system2("open", outFile)
  outFile

}
