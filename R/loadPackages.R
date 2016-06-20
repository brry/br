#' install package from local drive
#'
#' load packages I always like to have in the search path, update my own packages
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{updatePackage}}, \code{\link{installB}}
#' @keywords package
#' @importFrom utils packageDescription
#' @export
#' @examples
#'
#' \dontrun{
#' # Here's what you could write into your Rprofile.site:
#'
#' options(help_type="html")
#'
#' # set a site library
#' .Library.site <- "C:/Program Files/R/R-3.2.5/library"
#'
#' # Set CRAN repository
#' local({r <- getOption("repos")
#' r["CRAN"] <- "https://cran.rstudio.com" # 0-cloud
#' options(repos=r)})
#'
#' desktop <- "C:/Users/berry/Desktop"
#'
#' grDevices::windows.options(width=4.5, height=5)
#'
#' .trPaths <- paste(Sys.getenv('LOCALAPPDATA'),
#' '\\Temp\\Tinn-R', c('', 'search.txt', 'objects.txt',
#' 'file.r', 'selection.r', 'block.r','lines.r'),sep='\\')
#'
#' # Loading Packages
#' installB::loadPackages(ask=TRUE)
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
#' @param ask Logical. Prompt for input? If FALSE, loadPackages acts as if input is 2. DEFAULT: TRUE
#'
loadPackages <- function(ask=TRUE)
if(interactive())
{
# prompt user input:
if(ask)
  {
  message(" ---- Load packages? none (0), devtools+pbapply (1),\n       +berryFunctions (2/empty), +extremeStat (3/other). ")
  what <- readline("Load packages? 0/1/2/3. ")
  if(what=="") what <- "2"
  if(!what %in% c("0","1","2","3")) what <- "3"
  what <- as.integer(what)
  } else
  what <- 2
#
#  actual work
if(what>=1) {
  cat('-----------------------------------------------\nloadAndMessage("")')
  loadAndMessage("installB") # library("installB", quietly=TRUE)
  loadAndMessage("devtools")
  loadAndMessage("pbapply")
  }
if(what>=2) updatePackage("berryFunctions")
if(what>=3) updatePackage("extremeStat")
# working directory:
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
}
