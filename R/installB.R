#' install packages from local drive
#'
#' \bold{installB} removes function objects from workspace and tries to unload
#'       reverse dependencies. It then calls \code{devtools::\link[devtools]{install}}.\cr
#' \bold{installE} and \bold{installO} are shortcuts to installB with the
#'       default package="extremeStat" or "OSMscale".\cr
#' \bold{pathFinder} changes the path based on the computer used.\cr
#' \bold{loadAndMessage} calls \code{\link{require}} and gives verbose output.\cr
#' \bold{loadPackages} loads a number packages I always like to have in the search path.\cr
#' \bold{detach.all} unloads all packages in the search path.
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2014 - Sept 2016
#' @seealso \code{\link{packageDescription}}, \code{\link{read.dcf}}
#' @keywords package
#' @importFrom devtools install
#' @importFrom utils packageDescription sessionInfo
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
#' desktop <- "C:/Users/berry/Desktop"
#'
#' grDevices::windows.options(width=5.5, height=5)
#'
#' # Search path for Tinn R:
#' .trPaths <- paste(Sys.getenv('LOCALAPPDATA'),
#' '\\Temp\\Tinn-R', c('', 'search.txt', 'objects.txt',
#' 'file.r', 'selection.r', 'block.r','lines.r'),sep='\\')
#'
#' # Loading Packages
#' if(interactive()) installB::loadPackages(ask=FALSE)
#'
#'
#' # Here's what I have in my Rconsole file:
#' # Style This can be `yes' (for MDI) or `no' (for SDI).
#' MDI = no
#' # Dimensions (in characters) of the console.
#' rows = 15
#' columns = 100
#' # Dimensions (in characters) of the internal pager.
#' pgrows = 15
#' pgcolumns = 100
#' # Initial position of the console (pixels, relative to the workspace for MDI)
#' xconsole = 1088
#' yconsole = 716    # or 840
#' # Initial position of the graphics window
#' xgraphics = -5
#' ygraphics = 0
#' } # end dontrun
#'
#' @param package Package name. DEFAULT: "berryFunctions"
#' @param path Path containing package folder. DEFAULT: "S:/Dropbox/Public"
#' @param onlyupdate Logical. Only install if the version is outdated? DEFAULT: FALSE
#' @param load Logical. Also call loadAndMessage? DEFAULT: FALSE
#' @param quiet Logical for loadAndMessage: suppress messages like "package was built under R version xyz" in loadAndMessage
#' @param ask Logical for loadPackages. Prompt for input? If FALSE, loadPackages acts as if input is 2. DEFAULT: TRUE
#' @param \ldots Optional for installE and isntallO: path argument passed to installB


# ------------------------------------------------------------------------------

#' @export
installB <- function(
package="berryFunctions",
path="S:/Dropbox/Public",
onlyupdate=FALSE,
load=FALSE
)
{
# adjust path based on computer currently used:
path <- pathFinder(path)
#
# remove function objects from workspace
d <- dir(paste0(path, "/", package, "/R"))
d <- gsub(".r", "", d, fixed=TRUE)
d <- gsub(".R", "", d, fixed=TRUE)
l <- ls(globalenv())
rm(list=l[l %in% d], envir=globalenv())
# unload package dependencies to avoid unloadNamespace * not successful. Forcing unload." messages
try(unloadNamespace("extremeStat"), silent=TRUE)
try(unloadNamespace("OSMscale"), silent=TRUE)
#
doinst <- TRUE
if(onlyupdate) if(!requireNamespace(package, quietly=TRUE)) {doinst <- TRUE} else
{
  doinst <- FALSE
  # installed date/version:
  Vinst <- utils::packageDescription(package)[c("Date","Version")]
  # date in source code
  Vsrc <- read.dcf(file=paste0(path,"/",package, "/DESCRIPTION"), fields=c("Date","Version"))
  # install if outdated:
  if( as.Date(Vsrc[,"Date"]) > as.Date(Vinst$Date) | Vsrc[,"Version"] > Vinst$Version)
  doinst <- TRUE
}
# install
if(doinst) devtools::install(paste0(path, "/", package))
if(load) loadAndMessage(package)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname installB
installE <- function(...) installB(package="extremeStat", ...)
#' @export
#' @rdname installB
installO <- function(...) installB(package="OSMscale", ...)

# ------------------------------------------------------------------------------

#' @export
#' @rdname installB
pathFinder <- function(path) # adjust path based on computer currently used:
{
  # remove end slash
  while(substring(path, nchar(path))=="/")
    path <- substring(path, 1, nchar(path)-1)
  # work PC path change:
  if(!file.exists(path)) path <- gsub("S:", "C:/Users/boessenkool", path)
  # laptop linux path change:
  if(!file.exists(path)) path <- gsub("C:/Users/boessenkool", "/home/berry", path)
  #
  if(!file.exists(path)) stop("path does not exist. ", path)
  # Output:
  path
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname installB
loadAndMessage <- function(package, quiet=TRUE)
{
if(quiet) suppressWarnings(require(package, character.only=TRUE, quietly=TRUE))  else
                           require(package, character.only=TRUE, quietly=TRUE)
# prepare message
version <- utils::packageDescription(package)$Version
date <- utils::packageDescription(package)$Date
message("Loaded package ", format(package,width=15), "Version ",
          format(version,width=7), " from ", date)
}

# ------------------------------------------------------------------------------

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
4: + OSMscale")
  what <- readline("Load packages? 0/1/2/3. ")
  what <- as.integer(what)
} else
  what <- 2
#
#  actual work
if(what>=1) {
  cat('-----------------------------------------------\nloadAndMessage("")\n')
  loadAndMessage("installB") # library("installB", quietly=TRUE)
  loadAndMessage("devtools")
  loadAndMessage("pbapply")
}
if(what>=2) installB("berryFunctions", onlyupdate=TRUE, load=TRUE)
if(what>=3) installB("extremeStat",    onlyupdate=TRUE, load=TRUE)
if(what>=4) installB("OSMscale",       onlyupdate=TRUE, load=TRUE)
# working directory:
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname installB
detach.all <- function()
{
#pks <- rev(names(c(sessionInfo()$otherPkgs, sessionInfo()$loadedOnly)))
pks <- rev(names(sessionInfo()$otherPkgs))
message("detaching and unloading: ", toString(pks))
unload <- function(x) try(detach(x, character.only=TRUE, unload=TRUE))
dummy <- lapply(paste0('package:',pks), unload)
}

# ------------------------------------------------------------------------------


