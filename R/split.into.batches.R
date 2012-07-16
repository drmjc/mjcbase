#' split x into N batches each of a specific size.
#' 
#' @param x either a numeric vector, or a value specifying the length of the
#' data
#' @param N how many batches
#' @param batch.size if specified, then make the batches \code{batch.size} long; if
#' \code{NULL}, then the \code{batch.size} is determined from \code{x/N}.
#' 
#' @return A list with N entries, each entry being a vector of values from x.
#' If the \code{batch.size} is set too high given the length of x and N, then empty
#' batches are not included or returned
#' 
#' @author Mark Cowley, 9 June 2005
#' @export
split_into_batches <- function(x, N=NULL, batch.size=NULL) {
	if(length(x)==1)
		x <- c(1:x)

	stopifnot( N < length(x) )

	if(is.null(batch.size)) {
		batch.size <- round(length(x)/N,0)
		if((batch.size*N) < length(x))
			batch.size <- batch.size + 1
	}
	else if( is.null(N) ) {
		N <- round(length(x)/batch.size, 0)
		if((batch.size*N) < length(x))
			N <- N + 1
	}

	res <- list()
	for(i in 1:N) {
		if((i-1)*batch.size < length(x))
			res[i] <- list(x[c(((i-1)*batch.size+1):min(i*batch.size, length(x)))])
	}
	if(N*batch.size < length(x))
		warning("you have specified too small a batch.size to make N batches from x")

	return(res)
}
