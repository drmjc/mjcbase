#' Convert list of vectors to data.frame
#' 
#' Take a ragged \code{list} (ie a list of \code{vector}s of different lengths) and return a
#' \code{data.frame}, filling the shorter vectors with \code{NA}'s
#' 
#' @param x a list of vectors
#' 
#' @return a \code{data.frame}, filled with \code{NA}'s where necessary
#' 
#' @author Mark Cowley, 25/2/08
#' @export
#' 
#' @examples
#' list2df(list(letters[1:10], letters[11:15]))
#' 
list2df <- function(x) {
	lengths <- sapply(x, length)
	N <- max(lengths)
	res <- as.data.frame(matrix(NA, N, length(x)))
	if( is.null(names(x)) )
		names(x) <- 1:length(x)
	colnames(res) <- names(x)
	for(i in which(lengths > 0) ) {
		res[1:length(x[[i]]),i] <- x[[i]]
	}
	res
}

