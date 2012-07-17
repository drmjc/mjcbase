#' remove NA's from an object
#'
#' @note Defunct: \code{\link{na.omit}} is a better alternative.
#' 
#' @param x a 1D object
#' @return an object with no \code{NA}'s
#' @author Mark Cowley, 2012-07-16
#' @export
rm.na <- function(x) {
	.Defunct("na.omit")
	return(x[!is.na(x)])
}
