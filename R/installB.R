
# package doc ------------------------------------------------------------------

#' install packages from local drive
#' 
#' \bold{installB} removes function objects from workspace and tries to unload
#'       reverse dependencies. It then calls \code{devtools::\link[devtools]{install}}.\cr
#' \bold{installA} runs this for all my packages.\cr
#' \bold{pathFinder} changes the path based on the computer used.\cr
#' \bold{loadAndMessage} calls \code{\link{require}} and gives verbose output.\cr
#' \bold{loadPackages} loads a number of packages I always like to have in the search path.\cr
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2014 - Nov 2016
#' @seealso \code{\link{packageDescription}}, \code{\link{read.dcf}}
#' @keywords package
#' @importFrom devtools install
#' @importFrom utils packageDescription flush.console menu
#' @examples
#' \dontrun{
#' # Here's what you could write into your Rprofile.site:
#' 
#' options(help_type="html")
#' 
#' # Set CRAN repository
#' local({r <- getOption("repos")
#' r["CRAN"] <- "https://cran.rstudio.com" # 0-cloud
#' options(repos=r)})
#' 
#' # create character string  base::desktop
#' desktop <- "C:/Users/berry/Desktop" # linux "/home/berry/Desktop"
#' 
#' # Loading Packages
#' if(interactive()&requireNamespace("installB",quietly=TRUE)) installB::loadPackages(ask=FALSE)
#' 
#' 
#' # Search path for Tinn R:
#' .trPaths <- paste(Sys.getenv('LOCALAPPDATA'),
#' '\\Temp\\Tinn-R', c('', 'search.txt', 'objects.txt',
#' 'file.r', 'selection.r', 'block.r','lines.r'),sep='\\')
#' 
#' # if install.packages and download.files give HTTP status 403 forbidden:
#' options(url.method="libcurl")
#' 
#' } # end dontrun
#' 
#' @param package Package name. DEFAULT: NA (interactive selection)
#' @param path Path containing package folder. DEFAULT: "S:/Dropbox/Rpack"
#' @param force Logical. Even install if the version is not outdated? DEFAULT: FALSE
#' @param load Logical. Also call loadAndMessage? DEFAULT: TRUE
#' @param unloadrevdep Try to unload some common reverse dependencies? DEFAULT: TRUE
#' @param quiet Logical for loadAndMessage: suppress messages like "package was built under R version xyz" in loadAndMessage
#' @param ask Logical for loadPackages. Prompt for input? If FALSE, loadPackages acts as if input is 2. DEFAULT: TRUE
#' @param \ldots Optional for installE and isntallO: path argument passed to installB


# installB ---------------------------------------------------------------------

#' @export
#' @importFrom utils compareVersion
#' 
installB <- function(
package=NA,
path="S:/Dropbox/Rpack",
force=FALSE,
load=TRUE,
unloadrevdep=TRUE,
quiet=FALSE
)
{
# adjust path based on computer currently used:
path <- pathFinder(path)
#
# interactive package choice
if(is.na(package))
  {
  packs <- dir(path)
  packs <- packs[packs!="0-archive"]
  sel <- menu(packs, title=paste0("Which package would you like to install",
                                  if(!force)" (if outdated)", "?"))
  package <- packs[sel]
  }
# remove function objects from workspace:
d <- dir(paste0(path, "/", package, "/R"))
d <- gsub(".r", "", d, fixed=TRUE)
d <- gsub(".R", "", d, fixed=TRUE)
l <- ls(globalenv())
rm(list=l[l %in% d], envir=globalenv())
#
# unload package dependencies to avoid messages "unloadNamespace * not successful. Forcing unload."
if(unloadrevdep){
try(unloadNamespace("rfs"), silent=TRUE)
try(unloadNamespace("rdwd"), silent=TRUE)
try(unloadNamespace("mhmVis"), silent=TRUE)
try(unloadNamespace("extremeStat"), silent=TRUE)
try(unloadNamespace("OSMscale"), silent=TRUE)
}
#
# check if installed version is outdated:
if(force) doinst <- TRUE else
  {
  # installed date/version:
  Vinst <- suppressWarnings(utils::packageDescription(package)[c("Date","Version")])
  # if not yet installed, install anyways:
  if(all(is.na(Vinst))) Vinst <- list(Date="1900-01-01",Version="0")
  # date in source code
  descfile <- paste0(path,"/",package, "/DESCRIPTION")
  if(!file.exists(descfile)) stop("The file '", descfile, "' does not exist.")
  Vsrc <- read.dcf(file=descfile, fields=c("Date","Version"))
  # install if outdated:
  doinst <- as.Date(Vsrc[,"Date"]) > as.Date(Vinst$Date) | 
            compareVersion(Vsrc[,"Version"], Vinst$Version)==1
  }
# install
if(doinst)
  {
  message("installB will now install ", package)
  utils::flush.console()
  try(devtools::install(paste0(path, "/", package)))
  }
if(load) loadAndMessage(package, quiet=quiet)
}

# installA ---------------------------------------------------------------------

#' @export
#' @rdname installB
# @importFrom utils getFromNamespace
#' @importFrom git2r repository status

installA <- function(path="S:/Dropbox/Rpack", quiet=TRUE, ...)
{
path <- pathFinder(path)
packs <- dir(path)
packs <- packs[packs!="0-archive"]
packs <- packs[packs!="shapeInteractive"]
for(p in packs) installB(package=p, path=path, quiet=quiet, load=FALSE, ...)
for(p in packs) installB(package=p, path=path, quiet=quiet, unloadrevdep=FALSE, ...)
# check for unstaged git changes:
for(p in packs)
{
r <- git2r::repository(file.path(path, p), discover = TRUE)
st <- unlist(git2r::status(r))
if(!is.null(st)) message(length(st), " unstaged changes in ",format(p,width=15),
                         ": ", toString(st))
}
# getting uncommited changes via devtoools:::git_sync_status -> git2r::fetch
# fails due to failing ssh authentification
# When I stage changes, I almost always commit immediately, so this is enough

# Other git projects:
message("-----")
path <- normalizePath(file.path(path, ".."), winslash="/")
folders <- paste0("R/",c("course","latex","misc","PrecTemp","rclick","rhydro"))
for(p in c(folders,"Faith/chords"))
{
r <- git2r::repository(file.path(path, p), discover = TRUE)
st <- unlist(git2r::status(r))
if(!is.null(st)) message(length(st), " unstaged changes in ",format(p,width=15),
                         ": ", toString(st))
}
}


# pathFinder -------------------------------------------------------------------

#' @export
#' @rdname installB
pathFinder <- function(path="S:/Dropbox/Rpack") # adjust path based on computer currently used:
{
  # remove end slash
  while(endsWith(path,"/")) path <- substring(path, 1, nchar(path)-1)
  # work PC path change:
  if(!file.exists(path)) path <- gsub("S:", "C:/Users/boessenkool", path)
  # laptop linux path change:
  if(!file.exists(path)) path <- gsub("C:/Users/boessenkool", "/home/berry", path)
  #
  if(!file.exists(path)) stop("path does not exist. ", path)
  # Output:
  path
}

# loadAndMessage ---------------------------------------------------------------

#' @export
#' @rdname installB
loadAndMessage <- function(package, quiet=TRUE)
{
if(quiet) suppressMessages(suppressWarnings(
              require(package, character.only=TRUE, quietly=TRUE)))  else
              require(package, character.only=TRUE, quietly=TRUE)
# prepare message
packdesc <- utils::packageDescription(package)
if(all(is.na(packdesc))) return(paste0("loading package '", package, "' failed."))
version <- packdesc$Version
date    <- packdesc$Date
message("Loaded package ", format(package,width=15), "Version ",
          format(version,width=7), " from ", date)
}

# loadPackages -----------------------------------------------------------------

#' @export
#' @rdname installB
loadPackages <- function(ask=TRUE) if(interactive())
{
# prompt user input:
if(ask)
{
  message(" ---- Load packages?
0: none
1: installB+devtools+pbapply
2: +berryFunctions
3: +extremeStat
4: +OSMscale
5: +rdwd")
  what <- readline("Load packages? 0/1/2/3/4/5. ")
  what <- as.integer(what)
} else
  what <- 2
#
#  actual work
if(what>=1) {
  cat('-----------------------------------------------\nloadAndMessage("")\n')
  installB("installB") # library("installB", quietly=TRUE)
  loadAndMessage("devtools")
  loadAndMessage("pbapply")
}
if(what>=2) installB("berryFunctions")
if(what>=3) installB("extremeStat")
if(what>=4) installB("OSMscale")
if(what>=5) installB("rdwd")
# working directory:
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
}

# detachAll -------------------------------------------------------------------

#' @title Detach all packages on search path
#' @description Detach all packages on search path, as found by
#'              \code{\link{sessionInfo}()$otherPkgs}.
#'              It puts Berry's packages in descending dependency order.
#'              It tries \code{\link{unloadNamespace}} and 
#'              \code{\link{detach}(package:pkg, unload=TRUE)}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2016
#' @importFrom utils sessionInfo
#' @export

detachAll <- function()
{
#pks <- rev(names(c(sessionInfo()$otherPkgs, sessionInfo()$loadedOnly)))
pks <- names(sessionInfo()$otherPkgs)
if("visGPX" %in% pks | "mhmVis" %in% pks) pks <- c(pks, "OSMscale")
# put lowest dependency at end:
for(k in c("pbapply","extremeStat","rdwd","OSMscale","berryFunctions","devtools"))
  {
  i <- k==pks  ;  if( any(i) ) pks <- c(pks[!i], pks[i])
  }
message("detaching and unloading: ", toString(pks))
unload <- function(x) try({unloadNamespace(x)
                           detach(paste0('package:',x), character.only=TRUE, unload=TRUE)},
                          silent=TRUE)
lapply(pks, unload)
return(invisible(NULL))
}

# allFunctions -----------------------------------------------------------------

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

# isInstalled ------------------------------------------------------------------

#' @title Is a package installed and usable?
#' @description Is a package installed and usable?
#' @export
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

# packsNewR --------------------------------------------------------------------

#' Packages in new R installation
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2017
#' @export
#' @importFrom utils install.packages
#' @param \dots Arguments passed to \code{\link{install.packages}}
packsNewR <- function(...)
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
if(!isInstalled("pbapply")) install.packages("pbapply")

message("Checking ",length(packs)," packages for installation ...")
inst <- pbapply::pbsapply(packs, isInstalled) # [1:which(packs=="OpenStreetMap")]

if(any(!inst))
  {
  message("installB::packsNewR will install ",sum(!inst)," packages ...")
  Sys.sleep(1) # show message longer
  pbapply::pbsapply(packs[!inst], install.packages, ...)
  } else
  message("All packages listed in installB::packsNewR are installed.")

message("Done. To unload the ",length(packs)," checked packages, please restart R.")
}


# codeSpaces -------------------------------------------------------------------

#' @title Correct spaces in source code
#' @description Remove trailing spaces in source code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @export
#' @param path Package path, Character string. DEFAULT: "." (current getwd)
codeSpaces <- function(path=".")
{
path <- normalizePath(path, winslash="/")
if(endsWith(path, "/R"))         path <- substr(path, 1, nchar(path)-2)
if(endsWith(path, "/man"))       path <- substr(path, 1, nchar(path)-4)
if(endsWith(path, "/inst"))      path <- substr(path, 1, nchar(path)-5)
if(endsWith(path, "/vignettes")) path <- substr(path, 1, nchar(path)-10)
path <- paste0(path,"/R")
message("Correcting trailing spaces in files at ", path)
owd <- setwd(path) ; on.exit(setwd(owd))
remSpace <- function(file)
  {
  d <- d_orig <- readLines(file, warn=FALSE)
  trim <- function(x) {while(endsWith(x," ")) x <- substring(x, 1, nchar(x)-1); x}
  d <- sapply(d, trim, USE.NAMES=FALSE)
  d[d=="#'"] <- "#' "
  writeLines(d, file)
  sum(d_orig != d)
  }
nchanges <- sapply(dir(), remSpace)
if(any(nchanges!=0)) message("Number of lines changed:")
return(nchanges[nchanges!=0])
}

