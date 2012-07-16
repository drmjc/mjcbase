#' zeros ones
#' 
#' create a matrix of 0's or 1's, like the Matlab functions of the 
#' same name.
#'
#' @param nrow number of rows
#' @param ncol number of columns
#' @return a \code{matrix} of \code{dim(nrow, ncol)} of all 0's or 1's
#' @author Mark Cowley
#' @export
#' @rdname zeros-ones
#' @examples
#' zeros(5,3)
#' ones(5,3)
zeros <- function(nrow, ncol) {
	matrix(0, nrow, ncol)
}

#' @export
#' @rdname zeros-ones
ones <- function(nrow, ncol) {
	matrix(1, nrow, ncol)
}
