#' @title delete .DS_Store file on Mac OS
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2021
#' @export
#' @param folder  Folder in which all (incl recursive) .DS_Store files shall be deleted 
del_ds <- function(folder=".")
{
f <- dir(folder, pattern="^\\.DS_Store$", recursive=TRUE, full.names=TRUE, all.files=TRUE)
file.remove(f)
f
}
