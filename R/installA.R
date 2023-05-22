#' @title run installB on all my packages.
#' @importFrom git2r repository status

installA <- function(path="C:/Dropbox/Rpack", quiet=TRUE, ...)
{
path <- pathFinder(path)
packs <- list.dirs(path, recursive=FALSE, full.names=FALSE)
packs <- packs[packs!="0-archive"]
for(p in packs) installB(package=p, path=path, quiet=quiet, load=FALSE, ...)
for(p in packs) installB(package=p, path=path, quiet=quiet, unloadrevdep=FALSE, ...)
# check for unstaged git changes:
message("-----")

for(p in packs)
{
r <- try(git2r::repository(file.path(path, p), discover = TRUE), silent=TRUE)
if(inherits(r, "try-error")) 
 {
 if(p!="check") message("not a git repository: ", p) 
 }
else
 {
 st <- unlist(git2r::status(r))
 if(!is.null(st)) message(length(st), " unstaged changes in ",format(p,width=15),
                         ": ", toString(st))
 }
}
# getting uncommited changes via devtoools:::git_sync_status -> git2r::fetch
# fails due to failing ssh authentification
# When I stage changes, I almost always commit immediately, so this is enough

# Other git projects:
message("-----")
path <- normalizePath(file.path(path, ".."), winslash="/")
folders <- paste0("R/",c("course","latex","misc","rclick","rhydro"))
for(p in c(folders,"Faith/chords"))
{
r <- git2r::repository(file.path(path, p), discover = TRUE)
st <- unlist(git2r::status(r))
if(!is.null(st)) message(length(st), " unstaged changes in ",format(p,width=15),
                         ": ", toString(st))
}
message("-----")
}
