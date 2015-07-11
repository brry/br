# install current development version of package from folder
updatePackage <- function(
package="berryFunctions", # package name
path="S:/Dropbox/Public" # path containing package folder
)
{
if(interactive())
{
if(!require(package, character.only=TRUE, quietly=TRUE))
  stop("package ", package, " must be installed before updatePackage can work.")
# remove end slash
if(substring(path, nchar(path))=="/")
   path <- substring(path, 1, nchar(path)-1)
# installed date:
date_inst <- utils::packageDescription(package)$Date
# date in source code
date_source <- read.dcf(file=paste0(path,"/",package, "/DESCRIPTION"), fields="Date")
# install if updated:
if( as.Date(date_source) > as.Date(date_inst) )
  {
  require("devtools", quietly=TRUE) # now updatePackage can be in the Rprofile.site startup routine
  require("utils", quietly=TRUE) # to avoid warning: could not find function "available.packages"
  require("stats", quietly=TRUE) # the same for update
  installB(package, path)
  }
# load:
require(package, character.only=TRUE, quietly=TRUE)
# prepare message
version <- utils::packageDescription(package)$Version
date <- utils::packageDescription(package)$Date
message("Loaded package ", package, ", Version ", version, " from ", date ,".")
}
}

# Example Rprofile.site
#if(interactive())
#{
#library(installB)
#cat("-----------------------------------------------\n")
#updatePackage("berryFunctions", path="D:/Dropbox/Public")
#updatePackage(   "extremeStat", path="D:/Dropbox/Public")
#}




if(FALSE){
# install R package from github only if new one is available.
# Also load it and give some informative messages.
# needs devtools installed.
# works in Rprofile.site
# Berry Boessenkool, Sept 2014. I only got "getURL" to work with the help of
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/


#?packageStatus


# Update package if new version on github is available:
updateGithubPackage <- function(
user="BerryBoessenkool",  # username on github repository
package="berryFunctions", # package name
lookonline=TRUE, # should this function look online, or on local drive?
path="D:/Dropbox/Public", # path containing folder package, eg local dropbox
quietly=TRUE) # Suppress maskin warnings etc?
if(interactive())
 {
 # compare dates only if it is already installed:
 supall <- function(expr) suppressMessages(suppressWarnings(expr))
 supall(require(stats, quietly=TRUE))
 supall(require(utils, quietly=TRUE))
 supall(require(devtools, quietly=TRUE))
 if(supall(require(package, character=TRUE, quietly=TRUE)))
   {current <- utils::packageDescription(package)$Date
   if(lookonline)
     {# get date of package on github:
     u <- paste0("https://raw.github.com/", user, "/", package, "/master/DESCRIPTION")
     u2 <- RCurl::getURL(u, followlocation=TRUE, cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"))
     # position of "Date" in character string:
     d <- unlist(gregexpr("Date", u2))
     ongithub <- substr(u2, d+5, d+17)
     } else
     { # get date in local description file
     if(substring(path, nchar(path))=="/") path <- substring(path, 1, nchar(path)-1)
     u2 <- readLines(paste0(path,"/",package, "/DESCRIPTION"))
     ongithub <- substr(u2[grep("Date", u2)], 6, 17)
     }
   # whether newinstall is necessary
   newinstall <- as.Date(ongithub) > as.Date(current)
   } else newinstall <- TRUE
 if(newinstall)
  {
  devtools::install_github(paste0(user, "/", package))
  message("New version of ", package," installed from github repository.")
  }
 if(quietly) supall(require(package, character=TRUE, quietly=TRUE))
 else require(package, character=TRUE)
 # prepare message
 version <- utils::packageDescription(package)$Version
 date <- utils::packageDescription(package)$Date
 message("Loaded package ", package, ", Version ", version, " from ", date ,".")
}

}
