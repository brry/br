#' @title open course PDFs
#' @description open 4 course PDFs in default viewer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2019, Sept 2026
#' @param tut Logical: also open Tutorial pdf? DEFAULT: TRUE

course <- function(tut=TRUE) 
{
path <- sub("/Rpack", "/R/kurs/fp_slides_pdf", pathFinder()  )
pdfs <- dir(path, full.names=TRUE)
tpdf <- sub("/fp_slides_pdf", "/i_slides/FP_tutorial.pdf", path)
if(tut) pdfs <- c(pdfs, tpdf)
berryFunctions::openFile(pdfs)
}
