#' @title Is a function used in some code?
#' @description Report whether a function name is used in some code.
#'              Only returns TRUE if it is followed by "(" or included as pack::fun
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @importFrom utils head tail
#' @seealso \url{https://stackoverflow.com/questions/45691254}
#' @examples 
#' code <- "mean(pi); head(data.frame(A=1:5)); data_frame(7:9); do.call(base::print, list(pi))"
#' funs <- c("mean", "head", "head.data.frame",  "data.frame", "frame", "data_frame", "base::print")
#' data.frame(isfound=sapply(funs, funInCode, code=code), 
#'            shouldbefound=c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
#'            
#' funInCode("ABC::pack.dep", "ABC::pack_dep(A)")
#' 
#' @param funname Function name (character string)
#' @param code    Code (character string)
#' 
funInCode <- function(funname, code)
  {
  split <- strsplit(funname, "::")[[1]]
  fun1 <- tail(split,1)
  pack <- head(split,1)
  pack
  fun2 <- gsub(".", "\\.", fun1, fixed=TRUE)
  fun3 <- paste0("[^a-zA-Z0-9_\\.]", fun2, "\\(")
  out_name <- grepl(pattern=fun3, x=paste0(" ",code) )
  out_pack <- grepl(paste0("[^a-zA-Z0-9_\\.]", pack,"::",fun2), x=code)
  out_name | out_pack
  }

# funInCode2 <- function(code) {
# find_functions <- function(x, y=vector(mode="character", length=0)) 
# {
# if(is.symbol(x)) { if(is.function(eval(x))) c(y, as.character(x))
# } else {
#   if(!is.language(x)) NULL else c(y, unlist(lapply(x, find_functions)))
# }
# }
# unname(find_functions(parse(text=code)))
# }
