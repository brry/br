#' @title Find package path based on the computer used.
#' @export
#' @param path Path containing package folder. DEFAULT: "C:/Dropbox/Rpack"
pathFinder <- function(path="C:/Dropbox/Rpack")
{
# remove end slash
while(endsWith(path,"/")) path <- substring(path, 1, nchar(path)-1)
# Laptop path change:
if(!file.exists(path)) path <- gsub("C:", "S:", path)
# work Macbook path change:
if(!file.exists(path)) path <- gsub("S:", "~", path)
#
if(!file.exists(path)) stop("path does not exist. ", path)
# Output:
path
}
