#' load.dir
#' 
#' Load all the RDa objects within a directory
#'
#' @param dir a directory
#' @param pattern the suffix to identify RDa files. Default matches
#' any combination of upper and lower case from \dQuote{rda} to \dQuote{RDA}
#' @param envir The environment to load the objects to. Default = users workspace.
#' @return invisibly returns a vector of loaded objects, named
#'  by the file that they were loaded from
#' @author Mark Cowley, 2011-11-07
#' @export
#' @examples
#' \dontrun{
#' load.dir("./Rmisc")
#' }
load.dir <- function(dir, pattern="[Rr][dD][aA]$", envir=.GlobalEnv) {
	!missing(dir) && length(dir) == 1 && is.dir(dir) || stop("dir must be the path to a single valid directory")
	
	res <- list()
	rda <- dir(dir, pattern=pattern, full.names=TRUE)
	for(i in seq(along=rda)){
		res[[i]] <- load(rda[i], envir=envir)
	}
	names(res) <- rda
	
	invisible( res )
}
