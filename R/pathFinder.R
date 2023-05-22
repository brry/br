#' @export
#' @rdname installB
pathFinder <- function(path="C:/Dropbox/Rpack") # adjust path based on computer currently used:
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
