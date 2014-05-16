#' make a ragged list uniform in length
#' 
#' take a ragged list, that is a list of vectors of different lengths, and
#' make it into a non-ragged list, by appending \code{NA} to the end.
#'
#' @param l a list of vectors
#' @return a list of vectors of the same length
#' @author Mark Cowley, 2013-09-18
#' @export
#' @examples
#' l <- list(1:3, 1:5 ,1:10)
#' padNA(l)
#' 
padNA <- function(l) {
	maxL <- max(sapply(l, length))
	for(i in 1:length(l)) {
		x <- maxL - length(l[[i]])
		x > 0 || next
		l[[i]] <- c(l[[i]], rep(NA, x))
	}
	l
}
