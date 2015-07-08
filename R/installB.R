# shortcut for developing my package
# Berry Boessenkool, berry-b@gmx.de, Dec 2014

installB <- function(
package="berryFunctions", # package name
path="S:/Dropbox/Public") # path containing package folder
{
# work PC path change:
if(!file.exists(path)) substr(path, 1,1) <- "D"
# laptop linux path change:
if(!file.exists(path)) { substr(path, 1,1) <- "~" ; substr(path, 2,2) <- "" }
#
package <- deparse(substitute(package))
package <- gsub("\"", "", package, fixed=TRUE)
# remove function objects from workspace
d <- dir(paste0(path, "/", package, "/R"))
d <- gsub(".r", "", d, fixed=TRUE)
d <- gsub(".R", "", d, fixed=TRUE)
l <- ls(globalenv())
rm(list=l[l %in% d], envir=globalenv())
# unload package and dependencies to avoid "unloading not successfull" messages
#.....
# install
devtools::install(paste0(path, "/", package))
library(package, character.only=TRUE)
}