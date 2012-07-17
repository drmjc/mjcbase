#' Rotate values
#' 
#' Rotate a vector of values, starting from (1:N), (2:N,1), (3:N,1,2),
#' (4:N,1:3), \dots
#' 
#' @param x a vector of values
#' @param start.idx the index of the new first element in \code{x}
#' @return a vector same length and type as \code{x}
#' @author Mark Cowley 2008-05-13
#' @examples
#' rot(LETTERS[1:5],1)
#' rot(LETTERS[1:5],3)
#' rot(LETTERS[1:5],5)
#' @export
rot <- function(x, start.idx) {
	N <- length(x)
	
	if(start.idx == 1)
		x
	else if( start.idx == N )
		x[c(N,1:(N-1))]
	else
		x[c(start.idx:N, 1:(start.idx-1))]
}
