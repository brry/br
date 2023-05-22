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
