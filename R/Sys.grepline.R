#' Which lines do various patterns exist on?
#'
#' @param pattern a grep pattern. no spaces allowed!
#' @param file the file to search within
#' @param first logical: if \code{TRUE}, then only return the first match; if \code{FALSE},
#'   return all matches
#' @return a vector of indices. if pattern is not found, 0 is returned.
#' @author Mark Cowley, 2011-08-04
#' @export
Sys.grepline <- function(pattern, file, first=FALSE) {
	firstPattern <- if(first) "-m1" else ""
	cmd <- sprintf("grep %s -n %s '%s'", firstPattern, pattern, file)
    res <- system(cmd, intern=TRUE)
    if(nchar(res) == 0) return(0)
    else {
		res <- sub(":.*", "", res)
    	res <- as.numeric(res)
	}
    return(res)
}
