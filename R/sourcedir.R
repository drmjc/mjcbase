#' source all the .R files in a directory
#'
#' @param dir the path to a directory containing .R files
#' @param pattern a regular expression.  Only file names which match
#'        the regular expression will be returned. The default matches
#'        all files ending in \sQuote{.r} or \sQuote{.R}
#' @return nothing. the side effect is that the source code is imported.
#' @author Mark Cowley, 2012-03-07
#' @export
sourcedir <- function(dir, pattern="\\.[rR]") {
	files <- dir(dir, pattern=pattern, full.names=TRUE)
	for(file in files) {
		source(file)
	}
}
