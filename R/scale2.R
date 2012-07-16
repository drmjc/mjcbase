#' Function to linearly scale a vector of numbers (x) to be between min and
#' max.
#' 
#' @param x a numeric vector
#' @param min the minimum of the new range
#' @param max the maximum of the new range
#' 
#' @author Mark Cowley, 31 Oct 2005
#' @export
#' @examples
#' options(digits=2)
#' scale2(1:10,0,1)
#' # [1] 0.00 0.11 0.22 0.33 0.44 0.56 0.67 0.78 0.89 1.00
#'
scale2 <- function(x, min, max) {
	range.x <- diff(range(x, na.rm=TRUE)) ## want this to become 1.0
	if( range.x == 0 )
		return( rep(mean(c(min,max)), length(x)) )

	x2 <- (x-min(x, na.rm=TRUE))# * 1/range.x ## + min(x)

	new.range <- max - min
	res <- x2 * new.range + min
	res <- res / range.x
	res
}
