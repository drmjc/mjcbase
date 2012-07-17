#' Make a pair symmetrical
#' Convert a vector of 2 values into symmetrical ones. This is mostly used
#' during plotting to make xlim or ylim symmetrical about zero.
#' 
#' Typical usage scenarios are: \preformatted{ symmetricise( c(-0.6, 1.1) ) ->
#' c(-1.1, 1.1) symmetricise( c(0.6, 1.1) ) -> c(-1.1, 1.1) symmetricise(
#' c(-3.4, 1.1) ) -> c(-3.4, 3.4) }
#' 
#' @param x a numeric vector of length 2. See \code{\link{range}}
#' @return A numeric vector of length 2
#' @author Mark Cowley, 5 April 2006
#' @seealso \code{\link{range}}
#' @keywords manip
#' @examples
#' 
#' x <- rnorm(100)
#' r <- range(x)
#' r2 <- symmetricise(r)
#' plot(density(x), xlim=r2)
#' 
#' @export
symmetricise <- function(x) {
    reverse <- FALSE

    if( x[1] > x[2] )
        reverse <- TRUE

    if( abs(x[1]) != abs(x[2]) ) {
        min <- min(x, na.rm=TRUE)
        max <- max(x, na.rm=TRUE)
        if(min < 0 & max > 0) {
            min <- -max(abs(c(min, max)))
            max <- max(abs(c(min, max)))
        }
        else if( min > 0 ) {
            min <- -max
        }
        else if( max < 0) {
            max <- -min
        }

        # update x
        x <- c(min, max)
    }
    if(reverse)
        x <- rev(x)

    return( x )
}

