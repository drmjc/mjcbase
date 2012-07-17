#' Remove directories that are not empty.
#' 
#' Unlike \code{\link{file.remove}} which only removes files or empty directories, this one
#' will recursively delete a directory.
#' 
#' @param d a path to a directory (or a file)
#' @param debug logical: print helpful messages?
#' @return \code{TRUE} or \code{FALSE} based on success. the directory will be deleted. If
#'   \code{FALSE}, you will probably get a message like this:\cr
#' [1] FALSE 
#' Warning message: In file.remove("blah", recursive = TRUE) :
#'   cannot remove file 'blah', reason 'Directory not
#'   empty'\cr
#' This probably means that you have hidden files somewhere which
#'   prevent the directory from being deleted.  Warnings: if you have any
#'   hidden files, then this code will break. e.g. \dQuote{.DS_Store} or \dQuote{._xyz} files.
#' @author Mark Cowley, 2009-10-15
#' @export
dir.remove <- function(d, debug=FALSE) {
	if( !is.dir(d) || length(dir(d))==0 ) {
		if( debug ) cat("Deleting", shQuote(d), "\n")
		file.remove(d)
	}
	else {
		for(file in dir(d, full.names=TRUE)) {
			if( debug ) cat("Queuing", shQuote(file), "for deletion\n")
			dir.remove(file)
		}
		dir.remove(d)
	}
}
