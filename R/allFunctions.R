#' create and open a file with the content of all the functions in all my packages
#' 
#' @return file path
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Okt 2016
#' @export
#' @examples
#' # allFunctions()
#' 
#' @param package Name(s) of packages. If NA, all in my package folder. 
#'                DEFAULT: \code{berryFunctions::\link[berryFunctions]{packagePath}()}
#' @param \dots Arguments passed to \code{combineFiles} like quiet=TRUE
#' 
allFunctions <- function(
package=berryFunctions::packagePath(),
...
)
{
force(package)
owd <- getwd()
on.exit(setwd(owd))
setwd(pathFinder())
packNA <- all(is.na(package))
pname <- if(packNA) "allpack" else basename(package) 
if(packNA) package <- dir()[-1]
d <- dir(paste0(package,"/R"), full.names=TRUE)
if(any(grepl("rdwd", package))) d <- c(d, 
  "rdwd/vignettes/rdwd.Rmd",
  "rdwd/misc/vign/index.Rmd")

outFile <- paste0("0-archive/__All_functions_", pname, "_", Sys.Date(), ".r")
outFile <- berryFunctions::combineFiles(inFiles=d, outFile=outFile, ...)
berryFunctions::openFile(outFile)
outFile
}
