# install current development version of package from folder
updatePackage <- function(
package="berryFunctions", # package name
path="S:/Dropbox/Public" # path containing package folder
)
{
if(interactive())
{
if(!requireNamespace(package, quietly=TRUE))
  stop("package ", package, " must be installed before updatePackage can work.")
# remove end slash
if(substring(path, nchar(path))=="/")
   path <- substring(path, 1, nchar(path)-1)
# installed date/version:
Vinst <- utils::packageDescription(package)[c("Date","Version")]
# date in source code
Vsrc <- read.dcf(file=paste0(path,"/",package, "/DESCRIPTION"), fields=c("Date","Version"))
# install if outdated:
if( as.Date(Vsrc[,"Date"]) > as.Date(Vinst$Date) | Vsrc[,"Version"] > Vinst$Version)
  {
  require("devtools", quietly=TRUE) # now updatePackage can be in the Rprofile.site startup routine
  require("utils", quietly=TRUE) # to avoid warning: could not find function "available.packages"
  require("stats", quietly=TRUE) # the same for update
  installB(package, path)
  }
# load:
require(package, character.only=TRUE, quietly=TRUE)
# prepare message
version <- utils::packageDescription(package)$Version
date <- utils::packageDescription(package)$Date
message("Loaded package ", package, ", Version ", version, " from ", date ,".")
}
}

# Example Rprofile.site
#if(interactive())
#{
#library(installB)
#cat("-----------------------------------------------\n")
#updatePackage("berryFunctions", path="D:/Dropbox/Public")
#updatePackage(   "extremeStat", path="D:/Dropbox/Public")
#}
