#' create and open a file with the content of all the functions in all my packages
#'
#' @return file path
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Okt 2016
#' @export
#' @importFrom tools package_dependencies
#' @examples
#' # allFunctions()
#'
#' @param package Name(s) of packages. If NA, all in my package folder. DEFAULT: NA
#' @param \dots Arguments passed to \code{combineFiles} like quiet=TRUE
#'
allFunctions <- function(
package=NA,
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

  if(all(is.na(package))) package <- dir()[-1]
  d <- dir(paste0(package,"/R"), full.names=TRUE)

  outFile <- paste0("0-archive/__All_functions_", Sys.Date(), ".r")
  berryFunctions::combineFiles(inFiles=d, outFile=outFile, ...)
  if(!linux) system2("open", outFile) else system2("xdg-open", outFile)
  outFile

}


#' Packages in new R installation
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2017
#' @export
#' @importFrom utils install.packages
packsNewR <- function()
{
packs <- c("RColorBrewer", "berryFunctions", "rdwd", "foreign", "RCurl",
"zoo", "TeachingDemos", "ade4", "data.table", "microbenchmark", "nortest", "plotrix",
"rgl", "xts", "zoom", "zyp", "gstat", "numbers", "readxl", "lattice",
"gtools", "ncdf4", "pbapply",
"knitr", "devtools","rmarkdown", "roxygen2", "testthat", "extremeStat",

"rgdal", "rJava", "rgeos", "spatstat", "OSMscale", "geoR", "mapdata", "maps",
"raster", "RandomFields",
"maptools", "leaflet", "mapview", "sf", "dygraphs", "sp", "animation", "ggplot2",
"hexbin", "jpeg", "png", "rstudioapi"
)

message("Getting package dependencies ...")
basepacks <- c("base", "compiler", "datasets", "grDevices",
        "graphics", "grid", "methods", "parallel", "profile", "splines",
        "stats", "stats4", "tcltk", "tools", "translations", "utils")
deps <- tools::package_dependencies(packs, recursive=TRUE)
deps <- unique(unlist(deps))
deps <- deps[!deps %in% basepacks]
packs <- unique(c(deps,packs))

isinstalled <- function(p)    # is a package installed and usable?
 {
 suppressMessages(suppressWarnings(
 {out <- requireNamespace(p, quietly=TRUE)
 try(unloadNamespace(p), silent=TRUE)
 }))
 out
}
message("Checking ",length(packs)," packages for installation ...")
if(isinstalled("pbapply")) sapply <- pbapply::pbsapply
inst <- sapply(packs, isinstalled)
message("installB::packsNewR will install ",sum(!inst)," packages ...")
if(any(!inst)) install.packages(packs[!inst])
loadPackages(ask=FALSE)
}
