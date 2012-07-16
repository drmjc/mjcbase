#' Test if an Object is Sorted
#' 
#' Test if an object \emph{is} sorted, without the cost of sorting it.
#'
#' @inheritParams base::is.unsorted
#' 
#' @return
#' A length-one logical value.  All objects of length 0 or 1 are
#' sorted: the result will be \code{NA} for objects of length 2 or more
#' except for atomic vectors and objects with a class (where the \code{>=}
#' or \code{>} method is used to compare \code{x[i]} with \code{x[i-1]} for \code{i} in
#' \code{2:length(x)}).
#' 
#' @seealso \code{\link[base]{is.unsorted}} \code{\link[base]{sort}} \code{\link[base]{order}}
#' 
#' @note This function is designed for objects with one-dimensional
#'   indices, as described above.  \code{data.frame}, \code{matrices} and other
#'   \code{arrays} may give surprising results.
#'
#' @author Mark Cowley, 2012-07-16
#' @export
is.sorted <- function(x, na.rm = FALSE, strictly = FALSE) {
	return(!is.unsorted(x, na.rm = na.rm, strictly = strictly))
}
