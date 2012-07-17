#' Bind any number of lists together
#' Bind the elements within N lists into a larger list. Note this is not the same as
#' making a list with the N lists within it.
#' Consider 5 lists, each with 2 elements. this function will create a result with
#' 10 elements.
#' 
#' @param \dots at least 2 \code{list} objects.
#' @return one \code{list} with all the elements of \dots combined together
#' @author Mark Cowley, 1 Sept, 2005
#' @examples
#' a <- list(A=1:5, B=6:10, C=11:15)
#' b <- list(D=16:20, E=21:25, F=26:30)
#' lbind(a, b)
#' @export
lbind <- function(...) {
	args <- list(...) ## get the 'n' arguments
	names <- NULL

	idx <- 1
	res <- list()
	for(arg in 1:length(args)) {
		a <- args[[arg]]
		if( length(a) > 0 ) {
			res[(length(res)+1):(length(res)+length(a))] <- a
			# for(i in 1:length(a)) {
			# 	res[[idx]] <- a[[i]]
			# 	idx <- idx + 1
			# }
			names <- c(names, names(a))
		} ## lbind can be used to bind 'empty' lists thus the check for length(a) > 0
	}
	names(res) <- names

	return(res)
}
# CHANGELOG
# 2011-08-05: major updates to speed by avoiding the inner loop.

test_lbind <- function() {
	a <- list(A=1:5, B=6:10, C=11:15)
	b <- list(D=16:20, E=21:25, F=26:30)
	lbind(a, b)
}
