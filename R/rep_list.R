#' repeat x N times to make an N element list
#' 
#' @param x an object
#' @param N an integer > 1
#' 
#' @return a \code{list} or \code{length=N} where each element is \code{x}
#' @author Mark Cowley, 7 April 2006
#' @export
#' @examples
#' rep_list(letters[1:4], 5)
rep_list <- function(x, N=1) {
	res <- list()
	for(i in 1:N) res[[i]] <- x

	return( res )
}
