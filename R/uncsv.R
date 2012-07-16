#' uncsv
#' 
#' convert character vector where each element is composed of
#' comma separated elements, into a character vector.
#'
#' @param x a character vector of length >= 1
#' @param trim logical: trim whitespace? default=TRUE
#' 
#' @return a character vector of words, split on comma's
#' 
#' @author Mark Cowley, 2012-07-16
#' @export
#' @importFrom pwbc trim
#' @examples
#' uncsv("hello,quick,brown,fox")
#' # [1] "hello" "quick" "brown" "fox"	
uncsv <- function(x, trim=TRUE) {
	res <- unlist(strsplit(x, ","))
	if( trim )
		res <- trim(res)

	return( res )
}
