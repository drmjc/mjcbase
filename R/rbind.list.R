#' rbind the elements in a list together.
#' 
#' You can either supply many variables that are row-bindable, (ie are
#' the right dimension), or one list argument containing matrices that
#' can be row-binded
#' @param \dots either a single list of tables, or multiple tables to rbind, where table means
#' \code{matrix} or \code{data.frame}
#' @param sep currently unused
#' @return a large table with all elements in the list rbind-ed 
#' @author Mark Cowley, 5 April 2006
#' @export
rbind.list <- function(..., sep=".") {
    x <- list(...)
    if(length(x) == 1 & is.list(x))
        x <- x[[1]]

    if(is.null(names(x)))
        names(x) <- LETTERS[1:length(x)]

    res <- NULL
    #     names <- NULL
    for(i in 1:length(x)) {
        res <- rbind(res, x[[i]])
    #         names <- c(names, paste(rownames(x[[i]]), names(x)[i], sep=sep))
    }

    rownames(res) <- names(x)
    return(res)
}
