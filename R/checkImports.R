#' @title Check imports in source code
#' @description For each file in package/R, check whether all used functions are imported.
#'              There may be false positives: function names in comments or 
#'              within character strings are included as well.
#'              (Comment-only lines are ignored).
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @examples 
#' \dontrun{ # exclude from checks
#' library(berryFunctions)
#' setwd(paste0(pathFinder(), "/berryFunctions"))
#' imps <- checkImports()
#' openFile("checkImports.R")
#' for(n in names(imps)[!sapply(imps, is.null)]) openFile(n) ; rm(n)
#' unlink("checkImports.R")
#' }
#' 
#' @param path     Package path, Character string. DEFAULT: "." (current getwd)
#' @param filename Output file name. Text will be added, previous content remains.
#'                 DEFAULT: "checkImports.R"
#' 
checkImports <- function(path=".", filename="checkImports.R")
{
owd <- setwd(berryFunctions::packagePath(path))
on.exit(setwd(owd), add=TRUE)
filename <- berryFunctions::newFilename(filename)

# All the functions already listed in namespace importFrom entries:
ns <- readLines("NAMESPACE")
ns <- ns[substr(ns,1,10)=="importFrom"]
ns <- gsub("importFrom(", "", ns, fixed=TRUE)
ns <- gsub(")", "", ns, fixed=TRUE)
ns <- t(sapply(strsplit(ns,","),I))
ns <- as.data.frame(ns, stringsAsFactors=FALSE)
colnames(ns) <- c("pack","fun")

# include all functions in base R packages:
#ps <- .packages(); ps <- ps[ps!="base"]
ps <- c("compiler", "grDevices", "graphics", "grid", "methods", "parallel", 
        "splines", "stats", "tcltk", "tools", "utils") # "stats4",
ps <- lapply(ps, function(x) data.frame(pack=x, fun=getNamespaceExports(x),#ls(getNamespace(x)),
                                        stringsAsFactors=FALSE))
ns <- rbind(ns, do.call(rbind, ps)); rm(ps)
ns <- unique(ns)
# remove functions starting with non alpha-numeric symbols
ns <- ns[substr(ns$fun,1,1) %in% c(letters,LETTERS,0:9),]
# add package::function column:
ns$packfun <- paste(ns$pack,ns$fun, sep="::")

# Read the source code files:
codes <- sapply(dir("R/",full.names=TRUE), readLines, simplify=FALSE)

# interesting in development mode:
# head(ns)
# data.frame(loc=sort(sapply(codes,length)))

cat("\n##-----------------\n", file=filename, append=TRUE)
cat("## installB::checkImports,", as.character(Sys.time()), ",", length(codes), "files.\n\n", 
    file=filename, append=TRUE)

if(isInstalled("pbapply")) sapply <- pbapply::pbsapply
imps <- sapply(names(codes), function(fname)  # one minute in berryFunctions (103 files)
  {
  x <- codes[[fname]]
  # remove leading and trailing spaces:
  x2 <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
  xcode <- toString(x[substr(x2,1,1)!="#"])
  # get function names in source code
  fun <- base::sapply(ns$packfun, funInCode, code=xcode)
  fun <- names(fun[fun])
  if(length(fun)==0) return(invisible(NULL))
  # get associated packages:
  pack <- ns[ns$packfun %in% fun,]
  # exclude the ones already imported in file:
  isimp <- x[grepl("#' @importFrom",x)]
  isimp <- gsub("#' @importFrom ", "", isimp)
  isimp <- unlist(lapply(strsplit(isimp, " "), function(x) paste(x[1],x[-1],sep="::")))
  pack <- pack[!pack$packfun %in% isimp,]
  if(nrow(pack)==0) return(invisible(NULL))
  # write code to be added (@importFrom pack fun1 fun2) into file:
  out <- tapply(pack$fun, pack$pack, function(x) paste(sort(x), collapse=" "))
  out <- paste("#' @importFrom", names(out), out)
  out <- paste(out, collapse="\n")
  cat(fname, out, sep="\n", file=filename, append=TRUE)
  invisible(out)
  })
cat("\n## Done,", as.character(Sys.time()), file=filename, append=TRUE)
cat("\n##-----------------\n", file=filename, append=TRUE)
# output:
return(invisible(imps))
}
