#' Apply the mad function to each row of a matrix-like object
#' @param x a matrix-like object
#' @param \dots other arguments passed to apply
#' @return a numeric vector of MAD scores
#' @author Mark Cowley, 2008-12-16
#' @export
rowMAD <- function(x, ...) {
	apply(x, 1, mad, ...)
}


#' What is the range of values in each row?
#' @param x a matrix-like object
#' @return a numeric vector of ranges
#' @author Mark Cowley, 2009-04-09
#' @export
rowRange <- function(x) {
	rowapply(x, dRange)
}


#' What is the median in each row?
#' @param x a matrix-like object
#' @return a numeric vector of medians
#' @author Mark Cowley, 2009-04-09
#' @export
rowMedian <- function(x) {
	rowapply(x, median)
}


#' What is the delta-range, ie the distance between min and max?
#' @param x a vector
#' @param na.rm logical: remove the NA's prior to calculating min/max
#' @return a numeric(1) containing the range from min to max
#' @author Mark Cowley, 2009-06-04
#' @export
dRange <- function(x, na.rm=TRUE) {
	diff(range(x, na.rm=na.rm))
}


#' Convert rows in a table to a character vector, where each column contains
#' words which will be seperated by 'sep'
#'
#' @param x a matrix-like object
#' @param sep the separator character
#' @return a character vector of row elements pasted together, seperated by sep
#' @author Mark Cowley, 2009-07-29
#' @export
rowPaste <- function(x, sep=", ") {
	rowapply(x, paste, collapse=sep)
}
