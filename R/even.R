#' Check whether a vector or matrix of numbers are even.
#'
#' Credit goes to Gregory Warnes, since this is copied from \code{gtools}, 
#' so as to avoid loading an entire library for just one function
#'
#' @param x a vector or matrix of numbers
#' @return a logical vector or matrix, same size and dimensions as \code{x}
#' @author Gregory Warnes, Mark Cowley, 2/5/08
#' @seealso \code{\link{odd}}
#' @export
even <- function (x) {
	x == as.integer(x/2) * 2
}
