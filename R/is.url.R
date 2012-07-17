# Which entries are URL's.
#
# x must start with http://, ftp://, or file://
#     See also download.file
#
# Parameters:
#	x: a character vector.
#
# Value:
#	a logical vector of length x. values are TRUE if the entry looks like a URL. NB the URL's are not checked for 404 (dead links) errors.
#
# Mark Cowley, 2009-12-11
# 2010-06-09: updated to grepl and bitwise or
#


#' Which entries are URL's.
#' 
#' x must start with http://, ftp://, or file://
#' See also download.file
#' 
#' @param x a character vector.
#' @return a logical vector of length x. values are TRUE if the entry looks
#'   like a URL. NB the URL's are not checked for 404 (dead links) errors.
#' @author Mark Cowley, 2009-12-11
#' @export
is.url <- function(x) {
	grepl("^http://", x) | grepl("^ftp://", x) | grepl("^file://", x)
}
