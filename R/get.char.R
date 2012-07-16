#' get a specific character from a word
#'
#' @param x a \code{character} \code{vector} of length >= 1
#' @param idx the index into the characters, starting from 1
#' 
#' @return a character vector
#' @author Mark Cowley, 2012-07-16
#' @export
get.char <- function(x, idx) {
	sapply(strsplit(x, ""), "[", idx)
}

#' convert a word into a character vector
#'
#' @param x a \code{character} \code{vector} of \code{length == 1}
#' 
#' @return a charcter vector of words, each of length 1.
#' @author Mark Cowley, 2012-07-16
#' @export
to.char.array <- function(x) {
	unlist(strsplit(x, ""))
}
