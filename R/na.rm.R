#' remove the \code{NA}s from a vector
#'
#' @param x a vector with some \code{NA}s
#' @return a vector with no \code{NA}s in it
#' @export
#' @author Mark Cowley, 2011-07-18
#' @examples
#' x <- c(4,6,NA,8,9)
#' na.rm(x)
#' # c(4,6,8,9)
#' 
na.rm <- function(x) {
    return( x[!is.na(x)] )
}