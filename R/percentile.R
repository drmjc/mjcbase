#' percentile & quartile
#' 
#' Calculate the 11 pecentiles for the vector \code{x}, or
#' the 0th, 25th, 50th, 75th, 100th and mean value for the vector \code{x}
#' 
#' @param x a numeric vector
#' @param probs a vector or probabilities
#' @param \dots further arguments passed to \code{\link{quantile}}
#' 
#' @return \code{percentile}:  a named vector of length 11, names = the percentiles
#' 
#' @author Mark Cowley, 21 June 2005
#' @export
#' @rdname percentile
#' @examples
#'
#' x <- rnorm(25,0,1)
#' percentile(x)
#' quartile(x)
percentile <- function(x, probs=seq(0,1,0.1), ...) {
    return( quantile(x, probs=probs, ...) )
}

#' Calculate the 0th, 25th, 50th, 75th, 100th and mean value for the vector x
#' 
#' @return \code{quartile}: a named vector of length 6, names = the quartiles, \dQuote{mean}
#' @export
#' @rdname percentile
quartile <- function(x, ...) {
    return( c(quantile(x, ...), mean=mean(x)) )
}
