#' row-wise maximum
#'
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove NA's?
#' @return a vector of maxima, 1 per row in \code{x}
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' m <- matrix(1:25,5,5)
#' rowMax(m)
rowMax <- function(x, na.rm=TRUE) {
    apply(x, 1, max, na.rm=na.rm)
}

#' row-wise minimum
#'
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove NA's?
#' @return a vector of minima, 1 per row in \code{x}
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' m <- matrix(1:25,5,5)
#' rowMin(m)
rowMin <- function(x, na.rm=TRUE) {
    apply(x, 1, min, na.rm=na.rm)
}


#' column-wise maximum
#'
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove NA's?
#' @return a vector of maxima, 1 per column in \code{x}
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' m <- matrix(1:25,5,5)
#' colMax(m)
colMax <- function(x, na.rm=TRUE) {
    apply(x, 2, max, na.rm=na.rm)
}

#' column-wise minimum
#'
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove NA's?
#' @return a vector of minima, 1 per column in \code{x}
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' m <- matrix(1:25,5,5)
#' colMin(m)
colMin <- function(x, na.rm=TRUE) {
    apply(x, 2, min, na.rm=na.rm)
}

