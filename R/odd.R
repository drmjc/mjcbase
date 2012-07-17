#' Check whether a vector or matrix of numbers are odd.
#'
#' Credit goes to Gregory Warnes, since this is copied from \code{gtools}, 
#' so as to avoid loading an entire library for just one function
#'
#' @param x a vector or matrix of numbers
#' @return a logical vector or matrix, same size and dimensions as \code{x}
#' @author Gregory Warnes, Mark Cowley, 2/5/08
#' @seealso \code{\link{even}}
#' @export
odd <- function (x) {
	x != as.integer(x/2) * 2
}
