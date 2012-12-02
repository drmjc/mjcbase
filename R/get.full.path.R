#' Expand a file path to an absolute path
#' 
#' This function has more capabilities than \code{path.expand}, in that relative
#' paths (. and ./ and ../ and implicit ./) are also expanded, and more than \code{normalizePath}, in that
#' files with an implicit ./ are also expanded relative to the current working dir,
#' and files that don't currently exist can also expanded (see examples).
#' 
#' @param path A character vector of paths to files or directories
#' @param trailing.slash logical. if TRUE, and path is a directory, include a trailing \dQuote{/}
#' 
#' @return A character vector containing the full paths to files or directories given by \dQuote{file}
#' 
#' @author Mark Cowley
#' @export
#' @seealso \code{\link{normalizePath}}
#' 
#' @examples
#' \dontrun{
#' path.expand("~")
#' normalizePath("~")
#' get.full.path("~", trailing.slash=FALSE)
#' get.full.path("~", trailing.slash=TRUE)
#' 
#' path.expand("./")
#' normalizePath("./")
#' get.full.path("./")
#' 
#' path.expand("../")
#' normalizePath("../")
#' get.full.path("..")
#' 
#' #
#' # benefits over other approaches
#' # - file doesn't have to exist already
#' get.full.path("sgsdf.zip")
#' # [1] "/Users/marcow/src/R/mjcbase/R/sgsdf.zip"
#' path.expand("sgsdf.zip")
#' # [1] "sgsdf.zip"
#' normalizePath("sgsdf.zip")
#' # [1] "sgsdf.zip"
#' }
get.full.path <- function(path, trailing.slash=TRUE) {
	if( length(path) > 1 ) {
		res <- rep("", length(path))
		for(i in 1:length(path))
			res[i] <- get.full.path(path[i], trailing.slash=trailing.slash)
		return( res )
	}
	
	path <- path.expand(path) # handles the ~ expansion nicely.
	
	path[path==""] <- getwd()
	path[path=="."] <- getwd()
	if( grepl("^\\./", path) )
		path <- sub("^\\./", paste(getwd(),"/",sep=""), path) # relative path to getwd()
	else if( grepl("^\\../", path) ) {
		parent <- strsplit(getwd(), .Platform$file.sep)[[1]]
		ddCount <- 0
		while(grepl("^\\.\\./", path)) {
			ddCount <- ddCount + 1
			path <- sub("^\\.\\./", "", path)
		}
		parent <- parent[1:(length(parent)-ddCount)]
		parent <- paste(parent, collapse=.Platform$file.sep)
		path <- file.path(parent, path)
		# path <- sub("^\\.\\./", paste(getwd(),"/../",sep=""), path) # relative path to getwd()
	}
	else if( grepl("^/", path) ) {
		path <- path
	}
	else {
		path <- file.path(getwd(), path) # assumed to be in current working dir.
		# path <- sub("^([a-zA-Z0-9_.])", "./\\1", path) # assumed to be in current working dir.
	}

	## Append / if path is a directory
	if(		 is.dir(path)  &  trailing.slash & substring(path, nchar(path)) != "/" ) path <- paste(path, "/", sep="")
	else if( is.dir(path)  & !trailing.slash & substring(path, nchar(path)) == "/" ) path <- substring(path, 1, nchar(path)-1)

	return(path)
}
# CHANGELOG
# 7 Dec 2004: v1
# 2011-04-20: code overhaul/rewrite.
# 2011-06-21: allow >1 arguments.
# 2011-07-06: bug fix on "^./" paths
# 2012-10-12: resurrected this from defunct status, as normalizePath fails in 2 areas: files that don't exist, and files with implicit leading ./
test_get.full.path <- function(){
	setwd("~")
	get.full.path(".") == get.full.path("~")
	all(get.full.path(c(".", ".")) == get.full.path(c("~", "~")))
}
