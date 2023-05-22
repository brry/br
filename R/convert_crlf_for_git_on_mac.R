#' @title convert CRLF for git on Mac OS
#' @description convert CRLF for git on Mac OS
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2021
#' @seealso \url{https://www.reddit.com/r/RStudio/comments/jxx226/git_line_endings_in_r_studio_is_anyone_else/gcz8k31}
#' \url{https://stackoverflow.com/a/52694438}
#' @keywords file
#' @importFrom utils file.edit
#' @export
#' @param folder  Folder in which all files shall be processed 
#'                (unless file is given, then \code{folder} is ignored)
#' @param infile  File name
#' @param \dots   Arguments passed to \code{\link{dir}}
convert_crlf_for_git_on_mac <- function(folder=".", infile=NULL, ...)
{
stop("Just set:   git config --global core.autocrlf input")
}
