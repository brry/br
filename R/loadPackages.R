#' @export
#' @rdname installB
loadPackages <- function() if(interactive())
{
cat('-----------------------------------------------\nloadAndMessage("")\n')
  loadAndMessage("br")
  loadAndMessage("devtools")
  loadAndMessage("berryFunctions")
# working directory:
cat("-----------------------------------------------\n")
cat("getwd() : ", getwd(), "\n")
cat("-----------------------------------------------\n")
  checkOutdated("berryFunctions")  
}
