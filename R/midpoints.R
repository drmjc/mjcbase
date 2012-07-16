#' Determine the midpoints between a vector of numbers
#' 
#' @param x a numeric vector of length >= 2. if unsorted, then it
#'	will be sorted. \code{NA}'s will be excluded.
#' @return a numeric vector of midpoints, of length n-1
#' 
#' @author Mark Cowley, 30 June 2005
#' @export
#' @examples
#' midpoints(c(1,5,9))
#' # [1] 3 7
#'
midpoints <- function(x) {
	x <- x[!is.na(x)]
	if( is.unsorted(x) ) {
		x <- sort(x, decreasing=FALSE)
	}
	res <- rep(0, length(x)-1)
	for(i in 1:(length(x)-1)) {
		res[i] <- round(mean(c(x[i], x[i+1])), 0)
	}

	return(res)
}
