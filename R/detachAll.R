#' @title Detach all packages on search path
#' @description Detach all packages on search path, as found by
#'              \code{\link{sessionInfo}()$otherPkgs}.
#'              It puts Berry's packages in descending dependency order.
#'              It tries \code{\link{unloadNamespace}} and 
#'              \code{\link{detach}(package:pkg, unload=TRUE)}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2016
#' @importFrom utils sessionInfo

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
