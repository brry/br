#' @title Load a number of packages I always like to have in the search path.
#' @export
loadPackages <- function() if(interactive())
{
cat('-----------------------------------------------\nloadAndMessage("")\n')
  loadAndMessage("br")
  # devtools stuff in fromDevtools.R
  #loadAndMessage("berryFunctions")
# working directory:
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
  checkOutdated("berryFunctions")  
}
