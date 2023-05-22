#' Packages in new R installation
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2017
#' @export
#' @importFrom utils install.packages
#' @importFrom tools package_dependencies
#' @param \dots Arguments passed to \code{\link{install.packages}}
packsNewR <- function(...)
{
if(!isInstalled("pbapply")) install.packages("pbapply")
options(install.packages.check.source = "no")
packs <- c("RColorBrewer", "berryFunctions", "rdwd", "foreign", "RCurl",
"zoo", "TeachingDemos", "ade4", "data.table", "microbenchmark", "nortest", "plotrix",
"rgl", "xts", "zoom", "zyp", "gstat", "numbers", "readxl", "lattice",
"gtools", "ncdf4", "pbapply", "R.rsp", "intervals",
"knitr", "devtools","rmarkdown", "roxygen2", "testthat", "extremeStat",
"rJava", "spatstat", "OSMscale", "geoR", "mapdata", "maps",
"raster", "RandomFields", "plotKML", "rversions", "hunspell",
"leaflet", "mapview", "sf", "dygraphs", "sp", "animation", "ggplot2",
"hexbin", "jpeg", "png", "rstudioapi", "dwdradar", "rskey", "bookdown", "readODS",
"huxtable", "packrat", "rsconnect", "lmom", "quantmod", "osmdata", "rjson", "bit64", 
"rhub", "leaflet.extras", "DT", "pacman", "beepr", "miniUI", "googlesheets4", 
"openxlsx", "vioplot", "pdftools", "XML", "kableExtra"
)
message("Checking ",length(packs)," packages if they are installed ...")
inst <- pbapply::pbsapply(packs, isInstalled) 

if(any(!inst))
  {
  message("Getting package dependencies ...")
  deps <- tools::package_dependencies(packs[!inst], recursive=TRUE)
  deps <- unique(unlist(deps))
  basepacks <- c("base", "compiler", "datasets", "grDevices",
          "graphics", "grid", "methods", "parallel", "profile", "splines",
          "stats", "stats4", "tcltk", "tools", "translations", "utils")
  deps <- deps[!deps %in% c(basepacks,packs)]
  message("Checking ",length(packs)," dependencies if they are installed ...")
  deps <- pbapply::pbsapply(deps, isInstalled) 
  
  install_if_needed <- function(p) if(!isInstalled(p)) install.packages(p, ...)
  message("installB::packsNewR will install ",sum(!inst)," packages with ",
          sum(!deps), " dependencies ...")
  Sys.sleep(1) # show message longer
  pbapply::pbsapply(packs[!inst], install_if_needed, ...)
  } else
  message("All packages listed in installB::packsNewR are installed.")

message("Done. To unload the ",length(packs)," checked packages, please restart R.")
}
