#' truncate the values in a vector
#'
#' @param x a numeric vector
#' @param limits a numeric(2) indicating the min and max value. If either 
#'  is \code{NA}, then data will not be truncated at the min or max.
#' 
#' @return a numeric vector, same length as \code{x}, with values no
#' smaller than limits[1] and no larger than limits[2]
#' @author Mark Cowley, 2011-11-29
#' @export
#' @examples
#' x <- rnorm(20, 0, 2)
#' trunc(x, c(-1,1))
#' trunc(x, c(-1,NA))
#' trunc(x, c(NA,1))
trunc <- function(x, limits) {
	!missing(x) && is.numeric(x) || stop("x must be a 1D numeric vector")
	!missing(limits) && is.numeric(limits) && length(limits) == 2 || stop("limits must be a numeric(2) for min/max")
	
	if( is.na(limits[1]) ) limits[1] <- min(x, na.rm=TRUE)
	if( is.na(limits[2]) ) limits[2] <- max(x, na.rm=TRUE)
	limits[1] < limits[2] || stop("limits[1] > limits[2] !!")

	x[x<limits[1]] <- limits[1]
	x[x>limits[2]] <- limits[2]
	x
}
