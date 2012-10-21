#' Are all values equal?
#'
#' @param x a vector
#' @param na.rm logical: remove logicals?
#'
#' @return logical: \code{TRUE} if all elements are the same
#'
#' @export
#' @author Mark Cowley, 2011-07-18
#' @examples
#' x <- rep(1,5)
#' alleq(x)
#' alleq(letters)
#' # [1] FALSE
#' 
alleq <- function(x, na.rm=FALSE) {
	if (na.rm)
		x <- setdiff(x, NA)
	if (length(x) == 0)
		FALSE
	else if (length(x) == 1)
		TRUE
	else
		all(x[2:length(x)] == x[1])
}
# CHANGELOG
# 2012-08-28: added na.rm parameter
# 2012-10-21: documented na.rm parameter
