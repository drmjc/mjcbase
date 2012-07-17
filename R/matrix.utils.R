#
# matrix utilities
#

#' apply FUN to rows
#' @param x a \code{matrix} or \code{data.frame}
#' @param FUN a function
#' @param \dots further arguments passed to FUN
#' @return the result of running FUN on each row
#' @author Mark Cowley
#' @export
rowapply <- function(x, FUN, ...) {
    apply(x, MARGIN=1, FUN=FUN, ...)
}

#' apply FUN to columns
#' @param x a \code{matrix} or \code{data.frame}
#' @param FUN a function
#' @param \dots further arguments passed to FUN
#' @return the result of running FUN on each column
#' @author Mark Cowley
#' @export
colapply <- function(x, FUN, ...) {
    apply(x, MARGIN=2, FUN=FUN, ...)
}


#' Standard deviation of each row
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove \code{NA}'s?
#' @return the SD of each row
#' @author Mark Cowley
#' @export
rowSD <- function(x, na.rm=TRUE) {
    rowapply(x, sd, na.rm=na.rm)
}

#' Variance of each row
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove \code{NA}'s?
#' @return the variance of each row
#' @author Mark Cowley
#' @export
rowVar <- function(x, na.rm=TRUE) {
    rowapply(x, var, na.rm=na.rm)
}

#' Standard deviation of each column
#' @param x a \code{matrix} or \code{data.frame}
#' @param na.rm logical: remove \code{NA}'s?
#' @return the SD of each column
#' @author Mark Cowley
#' @export
colSD <- function(x, na.rm=TRUE) {
    colapply(x, sd, na.rm=na.rm)
}
