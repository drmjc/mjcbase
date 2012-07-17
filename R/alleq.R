#' Are all values equal?
#'
#' @param x a vector
#' @return logical: \code{TRUE} if all elements are the same
#' @export
#' @author Mark Cowley, 2011-07-18
#' @examples
#' x <- rep(1,5)
#' alleq(x)
#' alleq(letters)
#' # [1] FALSE
#' 
alleq <- function(x) {
    if(length(x) == 1)
        TRUE
    else
        all(x[2:length(x)] == x[1])
}

