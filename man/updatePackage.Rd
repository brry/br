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
if(interactive())
{
if(require(installB, quietly=TRUE)){
cat("-----------------------------------------------\n")
updatePackage("berryFunctions", path="S:/Dropbox/Public")
updatePackage(   "extremeStat", path="S:/Dropbox/Public")
}
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
}

}
}
\seealso{\code{\link{installB}}, \code{\link{packageDescription}}, \code{\link{read.dcf}} }
