#' Convert a vector x into a comma separated character(1).
#' 
#' Useful for pasting a list of entries in to a batch search engine.
#' You can change comma separator by changing the value of\code{sep}.
#' 
#' @param x a vector
#' @param sep the character to join the values with
#' @return a character(1) with each element of \code{x} joined together using \code{sep}
#' @author Mark Cowley, 15 May 2006
#' @seealso \code{\link{dump}} \code{\link{dput}}
#' @export
tocsv <- function(x, sep=", ") {
    paste(x, collapse=sep)
}
