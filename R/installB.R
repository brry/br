
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
#' # Speed up package installation
#' # https://blog.jumpingrivers.com/posts/2017/speed_package_installation/
#' options(Ncpus = parallel::detectCores()-1 )
#' 
#' # Loading Packages
#' try(installB::loadPackages())
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
#' 
installB <- function(
package=NA,
path="S:/Dropbox/Rpack",
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
message("-----")

for(p in packs)
{
r <- try(git2r::repository(file.path(path, p), discover = TRUE), silent=TRUE)
if(inherits(r, "try-error")) 
 {
 if(p!="check") message("not a git repository: ", p) 
 }
else
 {
 st <- unlist(git2r::status(r))
 if(!is.null(st)) message(length(st), " unstaged changes in ",format(p,width=15),
                         ": ", toString(st))
 }
}
# getting uncommited changes via devtoools:::git_sync_status -> git2r::fetch
# fails due to failing ssh authentification
# When I stage changes, I almost always commit immediately, so this is enough

# Other git projects:
message("-----")
path <- normalizePath(file.path(path, ".."), winslash="/")
folders <- paste0("R/",c("course","latex","misc","rclick","rhydro"))
for(p in c(folders,"Faith/chords"))
{
r <- git2r::repository(file.path(path, p), discover = TRUE)
st <- unlist(git2r::status(r))
if(!is.null(st)) message(length(st), " unstaged changes in ",format(p,width=15),
                         ": ", toString(st))
}
message("-----")
}



# checkOutdated ----------------------------------------------------------------

#' @export
#' @importFrom utils compareVersion packageDescription
#' 
checkOutdated <- function(
package,
path="S:/Dropbox/Rpack",
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



# pathFinder -------------------------------------------------------------------

#' @export
#' @rdname installB
pathFinder <- function(path="S:/Dropbox/Rpack") # adjust path based on computer currently used:
{
  # remove end slash
  while(endsWith(path,"/")) path <- substring(path, 1, nchar(path)-1)
  # Laptop path change:
  if(!file.exists(path)) path <- gsub("S:", "C:", path)
  # work PC path change:
  if(!file.exists(path)) path <- gsub("S:", "C:/Users/boessenkool", path)
  # laptop linux path change:
  if(!file.exists(path)) path <- gsub("C:/Users/boessenkool", "/home/berry", path)
  #
  if(!file.exists(path)) stop("path does not exist. ", path)
  # Output:
  path
}



# course -------------------------------------------------------------------

#' @title open R course PDF
#' @description open R course PDF in default viewer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2019
#' @export
#' @param pres Logical: Open presentation file with LaTeX onslide et al? DEFAULT: FALSE

course <- function(pres=FALSE) 
{
 path <- sub("/Rpack", "/R/course/RcourseBerry.pdf", pathFinder()  )
 if(pres) path <- sub(".pdf", " pres.pdf", path)
 berryFunctions::openFile(path)
}


# profilesite -------------------------------------------------------------------

#' @title open Rprofile.site
#' @description open Rprofile.site in default viewer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2019
#' @export
profilesite <- function() 
{
 path <- paste0( R.home("etc"), "/Rprofile.site")
 berryFunctions::openFile(path)
}



# newsdesc -------------------------------------------------------------------

#' @title open NEWS and DESCRIPTION files at package root
#' @description open NEWS and DESCRIPTION files at package root in default viewer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2019
#' @export
#' @param path Package dir. DEFAULT: "."
newsdesc <- function(path=".") 
{
path <- berryFunctions::packagePath(path)
# for the Rstudio version, don't call utils::file.edit explicitely
file.edit(paste0(path,"/DESCRIPTION")) 
file.edit(paste0(path,"/NEWS")) 
if(grepl("rdwd", path)) file.edit(paste0(path, "/misc/vign/index.Rmd"))
return(invisible(path))
}


# loadAndMessage ---------------------------------------------------------------

#' @export
#' @rdname installB
#' @param message Write custom message in loadAndMessage? DEFAULT: TRUE
loadAndMessage <- function(package, quiet=TRUE, message=TRUE)
{
if(quiet) suppressMessages(suppressWarnings(
              require(package, character.only=TRUE, quietly=TRUE)))  else
              require(package, character.only=TRUE, quietly=TRUE)
# prepare message
if(!message) return(invisible(NULL))
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
loadPackages <- function() if(interactive())
{
cat('-----------------------------------------------\nloadAndMessage("")\n')
  loadAndMessage("installB")
  loadAndMessage("devtools")
  loadAndMessage("pbapply")
  loadAndMessage("magrittr")
  loadAndMessage("berryFunctions")
# working directory:
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
  checkOutdated("installB")
  checkOutdated("berryFunctions")  
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
  if(grepl("rdwd", package)) d <- c(d, 
    "rdwd/vignettes/rdwd.Rmd",
    "rdwd/misc/vign/index.Rmd")

  outFile <- paste0("0-archive/__All_functions_", pname, "_", Sys.Date(), ".r")
  outFile <- berryFunctions::combineFiles(inFiles=d, outFile=outFile, ...)
  berryFunctions::openFile(outFile)
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
#' @importFrom tools package_dependencies
#' @param \dots Arguments passed to \code{\link{install.packages}}
packsNewR <- function(...)
{
if(!isInstalled("pbapply")) install.packages("pbapply")

packs <- c("RColorBrewer", "berryFunctions", "rdwd", "foreign", "RCurl",
"zoo", "TeachingDemos", "ade4", "data.table", "microbenchmark", "nortest", "plotrix",
"rgl", "xts", "zoom", "zyp", "gstat", "numbers", "readxl", "lattice",
"gtools", "ncdf4", "pbapply", "R.rsp", "intervals",
"knitr", "devtools","rmarkdown", "roxygen2", "testthat", "extremeStat",
"rgdal", "rJava", "rgeos", "spatstat", "OSMscale", "geoR", "mapdata", "maps",
"raster", "RandomFields", "plotKML", "rversions", "hunspell",
"maptools", "leaflet", "mapview", "sf", "dygraphs", "sp", "animation", "ggplot2",
"hexbin", "jpeg", "png", "rstudioapi", "dwdradar", "rskey", "bookdown", "readODS",
"huxtable", "packrat", "rsconnect", "lmom", "quantmod", "osmdata", "rjson"
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


# codeSpaces -------------------------------------------------------------------

#' @title Correct spaces in source code
#' @description Remove trailing spaces in source code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @export
#' @param path Package path, Character string. DEFAULT: "." (current getwd)
codeSpaces <- function(path=".")
{
path <- paste0(berryFunctions::packagePath(path),"/R")
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


# checkCodeLink -------------------------------------------------------------------

#' @title check for code{} and link{} statements
#' @description  check for code{} and link{} statements where the leading slash 
#' has been forgotten in the source code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @export
#' @param path Package path, Character string. DEFAULT: "." (current getwd)
checkCodeLink <- function(path=".")
{
path <- paste0(berryFunctions::packagePath(path),"/R")
rf <- dir(path, full.names=TRUE)
errors <- sapply(rf, function(f)
{
rl <- readLines(f)
err <- c(grep("(?<!/)code{", gsub("\\\\", "/", rl), perl=TRUE),
         grep("(?<!/)link{", gsub("\\\\", "/", rl), perl=TRUE)  )
err <- unique(err)
paste(err, rl[err], sep=": ")
}, simplify=FALSE)
errors <- errors[sapply(errors, length)>0]
if(length(errors)<1) 
  {
  message("No missing leading slashes found in ", path)
  return(invisible())
  }
errors <- lapply(errors, paste, collapse="\n")
errors <- paste(names(errors), unlist(errors), sep="\n")
errors <- paste(errors, collapse="\n\n")
message("missing leading slashes found in ", path, ":\n\n", errors)
}



# funInCode --------------------------------------------------------------------

#' @title Is a function used in some code?
#' @description Report whether a function name is used in some code.
#'              Only returns TRUE if it is followed by "(" or included as pack::fun
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @export
#' @importFrom utils head tail
#' @seealso \url{https://stackoverflow.com/questions/45691254}
#' @examples 
#' code <- "mean(pi); head(data.frame(A=1:5)); data_frame(7:9); do.call(base::print, list(pi))"
#' funs <- c("mean", "head", "head.data.frame",  "data.frame", "frame", "data_frame", "base::print")
#' data.frame(isfound=sapply(funs, funInCode, code=code), 
#'            shouldbefound=c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
#'            
#' funInCode("ABC::pack.dep", "ABC::pack_dep(A)")
#' 
#' @param funname Function name (character string)
#' @param code    Code (character string)
#' 
funInCode <- function(funname, code)
  {
  split <- strsplit(funname, "::")[[1]]
  fun1 <- tail(split,1)
  pack <- head(split,1)
  pack
  fun2 <- gsub(".", "\\.", fun1, fixed=TRUE)
  fun3 <- paste0("[^a-zA-Z0-9_\\.]", fun2, "\\(")
  out_name <- grepl(pattern=fun3, x=paste0(" ",code) )
  out_pack <- grepl(paste0("[^a-zA-Z0-9_\\.]", pack,"::",fun2), x=code)
  out_name | out_pack
  }

# funInCode2 <- function(code) {
# find_functions <- function(x, y=vector(mode="character", length=0)) 
# {
# if(is.symbol(x)) { if(is.function(eval(x))) c(y, as.character(x))
# } else {
#   if(!is.language(x)) NULL else c(y, unlist(lapply(x, find_functions)))
# }
# }
# unname(find_functions(parse(text=code)))
# } 
  

# checkImports -----------------------------------------------------------------

#' @title Check imports in source code
#' @description For each file in package/R, check whether all used functions are imported.
#'              There may be false positives: function names in comments or 
#'              within character strings are included as well.
#'              (Comment-only lines are ignored).
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @export
#' @examples 
#' \dontrun{ # exclude from checks
#' library(berryFunctions)
#' setwd(paste0(pathFinder(), "/berryFunctions"))
#' imps <- checkImports()
#' openFile("checkImports.R")
#' for(n in names(imps)[!sapply(imps, is.null)]) openFile(n) ; rm(n)
#' unlink("checkImports.R")
#' }
#' 
#' @param path     Package path, Character string. DEFAULT: "." (current getwd)
#' @param filename Output file name. Text will be added, previous content remains.
#'                 DEFAULT: "checkImports.R"
#' 
checkImports <- function(path=".", filename="checkImports.R")
{
owd <- setwd(berryFunctions::packagePath(path))
on.exit(setwd(owd), add=TRUE)
filename <- berryFunctions::newFilename(filename)

# All the functions already listed in namespace importFrom entries:
ns <- readLines("NAMESPACE")
ns <- ns[substr(ns,1,10)=="importFrom"]
ns <- gsub("importFrom(", "", ns, fixed=TRUE)
ns <- gsub(")", "", ns, fixed=TRUE)
ns <- t(sapply(strsplit(ns,","),I))
ns <- as.data.frame(ns, stringsAsFactors=FALSE)
colnames(ns) <- c("pack","fun")

# include all functions in base R packages:
#ps <- .packages(); ps <- ps[ps!="base"]
ps <- c("compiler", "grDevices", "graphics", "grid", "methods", "parallel", 
        "splines", "stats", "tcltk", "tools", "utils") # "stats4",
ps <- lapply(ps, function(x) data.frame(pack=x, fun=getNamespaceExports(x),#ls(getNamespace(x)),
                                        stringsAsFactors=FALSE))
ns <- rbind(ns, do.call(rbind, ps)); rm(ps)
ns <- unique(ns)
# remove functions starting with non alpha-numeric symbols
ns <- ns[substr(ns$fun,1,1) %in% c(letters,LETTERS,0:9),]
# add package::function column:
ns$packfun <- paste(ns$pack,ns$fun, sep="::")

# Read the source code files:
codes <- sapply(dir("R/",full.names=TRUE), readLines, simplify=FALSE)

# interesting in development mode:
# head(ns)
# data.frame(loc=sort(sapply(codes,length)))

cat("\n##-----------------\n", file=filename, append=TRUE)
cat("## installB::checkImports,", as.character(Sys.time()), ",", length(codes), "files.\n\n", 
    file=filename, append=TRUE)

if(isInstalled("pbapply")) sapply <- pbapply::pbsapply
imps <- sapply(names(codes), function(fname)  # one minute in berryFunctions (103 files)
  {
  x <- codes[[fname]]
  # remove leading and trailing spaces:
  x2 <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
  xcode <- toString(x[substr(x2,1,1)!="#"])
  # get function names in source code
  fun <- base::sapply(ns$packfun, funInCode, code=xcode)
  fun <- names(fun[fun])
  if(length(fun)==0) return(invisible(NULL))
  # get associated packages:
  pack <- ns[ns$packfun %in% fun,]
  # exclude the ones already imported in file:
  isimp <- x[grepl("#' @importFrom",x)]
  isimp <- gsub("#' @importFrom ", "", isimp)
  isimp <- unlist(lapply(strsplit(isimp, " "), function(x) paste(x[1],x[-1],sep="::")))
  pack <- pack[!pack$packfun %in% isimp,]
  if(nrow(pack)==0) return(invisible(NULL))
  # write code to be added (@importFrom pack fun1 fun2) into file:
  out <- tapply(pack$fun, pack$pack, function(x) paste(sort(x), collapse=" "))
  out <- paste("#' @importFrom", names(out), out)
  out <- paste(out, collapse="\n")
  cat(fname, out, sep="\n", file=filename, append=TRUE)
  invisible(out)
  })
cat("\n## Done,", as.character(Sys.time()), file=filename, append=TRUE)
cat("\n##-----------------\n", file=filename, append=TRUE)
# output:
return(invisible(imps))
}
