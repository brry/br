#' @title Check imports in source code
#' @description For each file in package/R, check whether all used functions are imported.
#'              There may be false positives: function names in comments or 
#'              within character strings are included as well.
#'              (Comment-only lines are ignored).
#'              Calls to imported functions that lack an explicit \code{pack::} prefix
#'              are listed as well (file:line), and returned as data.frame in 
#'              \code{attr(, "noprefix")} of the output. 
#'              Functions from base R packages (stats, utils, ...) are excluded from this list.
#'              Same-named functions in several packages are listed as \code{pack1|pack2}.
#'              Locally defined functions or arguments with the same name
#'              as an imported function are false positives.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @examples 
#' \dontrun{ # exclude from checks
#' library(berryFunctions)
#' setwd(paste0(pathFinder(), "/berryFunctions"))
#' imps <- checkImports()
#' for(n in names(imps)[!sapply(imps, is.null)]) openFile(n) ; rm(n)
#' unlink("checkImports.R")
#' }
#' 
#' @param path     Package path, Character string. DEFAULT: "." (current getwd)
#' @param filename Output file name. Will overwrite existing file.
#'                 DEFAULT: "checkImports.R"
#' 
checkImports <- function(path=".", filename="checkImports.R")
{
owd <- setwd(berryFunctions::packagePath(path))
on.exit(setwd(owd), add=TRUE)
message("Writing in ", filename)
cat("", file=filename)

# All the functions already listed in namespace importFrom entries:
# NAMESPACE is R syntax, so parse it (robust to roxygen2 line formatting):
nsexpr <- parse("NAMESPACE", keep.source=FALSE)
nsexpr <- nsexpr[vapply(nsexpr, function(e) identical(e[[1]], as.name("importFrom")), logical(1))]
ns <- do.call(rbind, lapply(nsexpr, function(e)
  {
  args <- vapply(as.list(e)[-1], as.character, character(1))
  data.frame(pack=args[1], fun=args[-1], stringsAsFactors=FALSE)
  }))
if(is.null(ns)) ns <- data.frame(pack=character(), fun=character(), stringsAsFactors=FALSE)
rm(nsexpr)

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

# Calls to imported functions without explicit pack:: prefix
cat("\n## Calls without pack:: prefix (file:line  pack::fun)\n", file=filename, append=TRUE)
# a function name may exist in several packages, so keep all candidates:
nsimp <- ns[!ns$pack %in% c("base", "compiler", "grDevices", "graphics", "grid", "methods", 
                            "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"),]
packs_of <- tapply(nsimp$pack, nsimp$fun, function(x) paste(sort(unique(x)), collapse="|"))
noprefix <- lapply(names(codes), function(fname)
  {
  x <- codes[[fname]]
  x2 <- sub("^[[:space:]]*", "", x)
  x[substr(x2,1,1)=="#"] <- "" # keeps line numbers intact
  # function names followed by "(", not preceded by ::, $, @, or name characters
  m <- gregexpr("(?<![A-Za-z0-9_.:$@])[A-Za-z.][A-Za-z0-9._]*\\(", x, perl=TRUE)
  hits <- regmatches(x, m)
  line <- rep(seq_along(x), lengths(hits))
  fun <- sub("\\($", "", unlist(hits))
  keep <- fun %in% names(packs_of)
  if(!any(keep)) return(NULL)
  data.frame(file=fname, line=line[keep], fun=fun[keep], 
             pack=unname(packs_of[fun[keep]]), stringsAsFactors=FALSE)
  })
names(noprefix) <- names(codes)
noprefix <- do.call(rbind, noprefix)
if(!is.null(noprefix))
  {
  rownames(noprefix) <- NULL
  cat(paste0(noprefix$file, ":", noprefix$line, "  ", noprefix$pack, "::", noprefix$fun),
      sep="\n", file=filename, append=TRUE)
  }
cat("\n## Done,", as.character(Sys.time()), file=filename, append=TRUE)
cat("\n##-----------------\n", file=filename, append=TRUE)
try(berryFunctions::openFile(filename), silent=TRUE)
# output:
attr(imps, "noprefix") <- noprefix
return(invisible(imps))
}
