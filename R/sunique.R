#' sort and uniquify a vector
#' 
#' @param x a vector
#' @param \dots additional arguments passed to \code{\link{unique}}
#' @return a vector of unique, sorted values
#' @author Mark Cowley, 23/3/07
#' @export
#' @examples
#' sunique(c("b", "z", "a", "z", NA))
#' # [1] "a" "b" "z"
sunique <- function(x, ...) {
    if( is.list(x) )
        lapply(x, sunique, ...)
    else
        sort(unique(x), ...)
}
