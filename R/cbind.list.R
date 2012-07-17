#' column bind the elements in a list together.
#' 
#' You can either supply many variables that are col-bindable, (ie are
#' the right dimension), or one list argument containing matrices that
#' can be col-binded.
#' 
#' @param \dots either 1 list of \code{cbind}-able objects, or at least 2 \code{cbind}-able objects
#' @param sep the seperator character to make the col.names unique, only if necessary
#' @author Mark Cowley, 5 April 2006
#' @export
cbind.list <- function(..., sep=".") {
    x <- list(...)
    if(length(x) == 1 & is.list(x))
        x <- x[[1]]

    if(is.null(names(x)))
        names(x) <- LETTERS[1:length(x)]

    res <- NULL
    names <- NULL
    for(i in 1:length(x)) {
        if(i == 1)
            res <- x[[i]]
        else
            res <- cbind(res, x[[i]])
        names <- c(names, 
                    ifelse(is.matrix.like(x[[i]]), 
                        paste(colnames(x[[i]]), names(x)[i], sep=sep),
                        names(x)[i]))
    }
    colnames(res) <- names

    return(res)
}
