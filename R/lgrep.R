#' grep for a pattern in a list of values.
#' 
#' Why? grep normally tries to find a pattern in a vector of values; what if
#' you have a list of vectors, and want to know in which vector does your value
#' appear??
#' 
#' @param pattern a character(1)
#' @param x a list, usually where each element is a character vector.
#' @param \dots arguments passed to \code{grepl}
#' @return returns the index into the list that contains your pattern.
#' @author Mark Cowley, 2009-10-16
#' @export
lgrep <- function(pattern, x, ...) {
	res <- sapply(x, function(vec) grepl(pattern, vec, ...))
	which(res)
}
