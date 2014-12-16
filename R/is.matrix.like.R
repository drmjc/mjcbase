#' is x a \code{matrix} or \code{data.frame}?
#'
#' @param x an R object
#' @return logical: \code{TRUE} if \code{x} is a \code{matrix} or \code{data.frame}; 
#' \code{FALSE} otherwise
#' @export is.matrix.like
#' @author Mark Cowley, 2011-07-18
#' @examples
#' m <- matrix(1:25,5)
#' is.matrix.like(m)
#' df <- as.data.frame(m)
#' is.matrix.like(df)
#' is.matrix.like(letters)
#' # FALSE
is.matrix.like <- function(x) {
    is.matrix(x) | is.data.frame(x)
}
