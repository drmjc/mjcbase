#' From a character vector of length >= 1, determine the first or last n characters.
#' 
#' If n is larger then the length of the word, the entire word is returned
#' 
#' @param x a character vector
#' @param n the number of characters to be returned
#' @author Mark Cowley, 2009-01-09
#' @export
#' @rdname str_ends
str_right <- function(x, n) {
	str.ends <- nchar(x)
	str.starts <- str.ends - n + 1
	if( any(str.starts < 1) )
		str.starts[str.starts < 1] <- 1
	res <- rep("", length(x))
	for(i in 1:length(x)) {
		res[i] <- substr(x[i], str.starts[i], str.ends[i])
	}
	res
}

#' @export
#' @rdname str_ends
str_left <- function(x, n) { 
	str.starts <- rep(1, length(x))
	str.ends <- rep(n, length(x))
	str.lengths <- nchar(x)
	str.ends <- apply(cbind(str.ends, str.lengths), 1, min)
	
	res <- rep("", length(x))
	for(i in 1:length(x)) {
		res[i] <- substr(x[i], str.starts[i], str.ends[i])
	}
	res
}
