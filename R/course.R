#' @title open R course PDF
#' @description open R course PDF in default viewer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2019
#' @param pres Logical: Open presentation file with LaTeX onslide et al? DEFAULT: FALSE

course <- function(pres=FALSE) 
{
path <- sub("/Rpack", "/R/course/RcourseBerry.pdf", pathFinder()  )
if(pres) path <- sub(".pdf", " pres.pdf", path)
berryFunctions::openFile(path)
}
