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
  berry <- Sys.info()["user"]=="berry"
  linux <- Sys.info()["sysname"]=="Linux"
  work <- Sys.info()["nodename"]=="GK-PC-2"
  if(berry&!linux)           setwd("S:/Dropbox/Rpack")
  if(berry& linux)  setwd("/home/berry/Dropbox/Rpack")
  if(work) setwd("C:/Users/boessenkool/Dropbox/Rpack")
  
  d <- dir()[-1]
  d <- dir(paste0(d,"/R"), full.names=TRUE)

  outFile <- paste0("0-archive/__All_functions_", Sys.Date(), ".r")
  berryFunctions::combineFiles(inFiles=d, outFile=outFile, ...)
  if(!linux) system2("open", outFile) else system2("xdg-open", outFile)
  outFile

}
