#' Convert a vector x into a comma separated character(1).
#' 
#' Useful for pasting a list of entries in to a batch search engine.
#' You can change comma separator by changing the value of \code{sep}.
#' \code{toscsv} is read as to-semicolon-separated-values.
#' 
#' @param x a vector
#' @param na.replace replace all \code{NA}'s with a value, such as \dQuote{}. default=\code{NA},
#' ie don't replace \code{NA}'s which results in a text string of \dQuote{..., NA, ...}
#' @param sep the character to join the values with
#' 
#' @return a character(1) with each element of \code{x} joined together using \code{sep}
#' 
#' @author Mark Cowley, 15 May 2006
#' @seealso \code{\link{dump}} \code{\link{dput}}
#' @export
#' @rdname tocsv
tocsv <- function(x, na.replace=NA, sep=", ") {
	if( !is.na(na.replace) ) x <- ifelse(is.na(x),na.replace,x)
    paste(x, collapse=sep)
}
# CHANGELOG
# 2012-08-09: added na.replace parameter

#' @export
#' @rdname tocsv
toscsv <- function(x, na.replace="") {
	tocsv(x, na.replace=na.replace, sep=";")
}
