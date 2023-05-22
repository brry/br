#' @title check if installed package is outdated.
#' @importFrom utils compareVersion packageDescription
#' @param package Package name
#' @param path Path containing package folder. DEFAULT: "C:/Dropbox/Rpack"
#' @param quiet suppress messaging.

#' 
checkOutdated <- function(
  package,
  path="C:/Dropbox/Rpack",
  quiet=FALSE
)
{
try(library(package, character.only=TRUE), silent=TRUE)
# adjust path based on computer currently used:
path <- pathFinder(path)
# check if installed version is outdated:
# installed date/version:
Vinst <- suppressWarnings(utils::packageDescription(package)[c("Date","Version")])
# if not yet installed, consider it outdated:
if(all(is.na(Vinst))) Vinst <- list(Date="1900-01-01",Version="0")
# date in source code
descfile <- paste0(path,"/",package, "/DESCRIPTION")
if(!file.exists(descfile)) stop("The file '", descfile, "' does not exist.")
Vsrc <- read.dcf(file=descfile, fields=c("Date","Version"))
# install if outdated:
outdated <- as.Date(Vsrc[,"Date"]) > as.Date(Vinst$Date) | 
 compareVersion(Vsrc[,"Version"], Vinst$Version)==1
if(outdated & !quiet) message("'", package, "' is outdated.\n- Installed: ", 
                              Vinst$Version," (",Vinst$Date,")\n- Source   : ",
                              Vsrc[,"Version"], " (",Vsrc[,"Date"],")")
return(invisible(outdated))
}
