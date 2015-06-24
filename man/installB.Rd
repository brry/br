\name{installB}
\alias{installB}
\title{install package from my local drive}
\description{Basically just an enhanced shortcut to \code{devtools::\link[devtools]{install}}.}
\usage{installB(package="berryFunctions", path="S:/Dropbox/Public")}
\arguments{
  \item{package}{package name. DEFAULT: "berryFunctions"}
  \item{path}{path containing package folder. DEFAULT: "S:/Dropbox/Public")}
}
\details{Removes function objects from workspace and (toDo!) unloads reverse dependencies first.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Dec 2014}
\examples{
# installB()
}
