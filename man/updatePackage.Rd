\name{updatePackage}
\alias{updatePackage}
\title{install package from my local drive}
\description{Calls \code{\link{installB}} if the package date in the source code has been changed.}
\usage{updatePackage(package="berryFunctions", path="S:/Dropbox/Public" )}
\arguments{
  \item{package}{package name. DEFAULT: "berryFunctions"}
  \item{path}{path containing package folder. DEFAULT: "S:/Dropbox/Public"}
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\examples{
\dontrun{
# Here's what you could write into your Rprofile.site:

options(help_type="html")

# set a site library
.Library.site <- "C:/Program Files/R/R-3.2.5/library"

# Set CRAN repository
local({r <- getOption("repos")
r["CRAN"] <- "https://cran.rstudio.com" # 0-cloud
options(repos=r)})

desktop <- "C:/Users/berry/Desktop"

grDevices::windows.options(width=4.5, height=5)

.trPaths <- paste(Sys.getenv('LOCALAPPDATA'),
'\\Temp\\Tinn-R', c('', 'search.txt', 'objects.txt',
'file.r', 'selection.r', 'block.r','lines.r'),sep='\\')


# Loading Packages
if(interactive())
{
message(" ---- Load own packages? y/n/none. Empty==n. ")
inputQuestionUpdatePackage <- readline("Load own packages? y/n/none. Empty==n. ")
if(! match(inputQuestionUpdatePackage, c("","n","no", "none"), nomatch=0) > 0)
  if(require(installB, quietly=TRUE))
  {
  cat("-----------------------------------------------\n")
  updatePackage("berryFunctions", path="S:/Dropbox/Public")
  updatePackage(   "extremeStat", path="S:/Dropbox/Public")
  }
if(inputQuestionUpdatePackage != "none")
  {
  require("devtools", quietly=TRUE)
  message("Loaded package devtools,    Version ",
     utils::packageDescription("devtools")$Version,
     " from ", utils::packageDescription("devtools")$Date ,".")
  require("pbapply", quietly=TRUE)
  message("Loaded package pbapply,     Version ",
     utils::packageDescription("pbapply")$Version,
    " from ", utils::packageDescription("pbapply")$Date ,".")
  }
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
} # End loading packages


### Rconsole file:
# Style This can be `yes' (for MDI) or `no' (for SDI).
MDI = no
# Dimensions (in characters) of the console.
rows = 15
columns = 100
# Dimensions (in characters) of the internal pager.
pgrows = 15
pgcolumns = 100
# Initial position of the console (pixels, relative to the workspace for MDI)
xconsole = 1088
yconsole = 716
# Initial position of the graphics window
xgraphics = -5
ygraphics = 0

}
}
\seealso{\code{\link{installB}}, \code{\link{packageDescription}}, \code{\link{read.dcf}} }
