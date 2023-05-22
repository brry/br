#' @title install packages from local drive
#' @description
#' Remove function objects from workspace and try to unload reverse dependencies. 
#' Then call \code{devtools::\link[devtools]{install}}.\cr
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2014 - 2023
#' @seealso \code{\link{packageDescription}}, \code{\link{read.dcf}}
#' @keywords package
#' @importFrom devtools install
#' @importFrom utils packageDescription flush.console menu
#' @examples
#' \dontrun{
#' # Here's what you could write into your Rprofile.site:
#' 
#' # Loading Packages
#' try(br::loadPackages())
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
#' # Speed up package installation
#' # https://blog.jumpingrivers.com/posts/2017/speed_package_installation/
#' options(Ncpus = parallel::detectCores()-1 )
#' 
#' # if install.packages and download.files give HTTP status 403 forbidden:
#' options(url.method="libcurl")
#' 
#' } # end dontrun
#' 
#' @param package Package name. DEFAULT: NA (interactive selection)
#' @param path Path containing package folder. DEFAULT: "C:/Dropbox/Rpack"
#' @param force Logical. Even install if the version is not outdated? DEFAULT: FALSE
#' @param load Logical. Also call loadAndMessage? DEFAULT: TRUE
#' @param unloadrevdep Try to unload some common reverse dependencies? DEFAULT: TRUE
#' @param quiet Suppress messages? DEFAULT: FALSE
#' @param \ldots passed to \code{devtools::\link[devtools]{install}}
#' @export
#' 
installB <- function(
package=NA,
path="C:/Dropbox/Rpack",
force=FALSE,
load=TRUE,
unloadrevdep=TRUE,
quiet=FALSE,
...
)
{
# adjust path based on computer currently used:
path <- pathFinder(path)
#
# interactive package choice
if(is.na(package))
  {
  packs <- list.dirs(path, full.names=FALSE, recursive=FALSE)
  packs <- packs[!packs %in% c(".Rproj.user", "0-archive")]
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
try(unloadNamespace("rdwd"), silent=TRUE)
try(unloadNamespace("mhmVis"), silent=TRUE)
try(unloadNamespace("extremeStat"), silent=TRUE)
try(unloadNamespace("OSMscale"), silent=TRUE)
try(unloadNamespace(package), silent=TRUE) # if devtools::load_all was used, 
# packageDescription would otherwise not return the date of the actually installed package
}
#
# check if installed version is outdated:
doinst <- if(force) TRUE else checkOutdated(package, path, quiet=TRUE)
# install
if(doinst)
  {
  message("installB will now install ", package)
  utils::flush.console()
  try(devtools::install(paste0(path, "/", package), build_vignettes=TRUE, ...))
  }
if(load) loadAndMessage(package, quiet=quiet)
}
