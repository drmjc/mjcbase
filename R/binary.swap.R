#' swap the 1's and 0's around
#'
#' @param x a numeric vector of 0/1
#' @return a numeric vector like the input, but with 1's and 0's swapped.
#' @author Mark Cowley, 2011-09-02
#' @export
#' @importFrom methods is
#' @examples
#' input <- c(0,1,0,1)
#' binary.swap(input)
#' \dontrun{
#' binary.swap(c(input, NA))
#' # Error in binary.swap(c(input, NA)) : x must be 0's and 1's
#' }
binary.swap <- function(x) {
	is(x, "numeric") || stop("x must be a numeric vector")
	all(x %in% c(0,1)) || stop("x must be 0's and 1's")
	
	res <- rep(0, length(x))
	res[x==0] <- 1
	res
}
