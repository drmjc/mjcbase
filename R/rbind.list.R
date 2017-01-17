#' rbind the elements in a list together.
#' 
#' You can either supply many variables that are row-bindable, (ie are
#' the right dimension), or one list argument containing matrices that
#' can be row-binded
#'
#' rownames will be set to <list element name>.<original row name>
#'
#' Often when rbind-ing tables of data together, you want to tag all rows 
#' from each table with the sample to which they belong. You can specify
#' \code{name2colname}, which will add a new column and track the origin
#' of those rows.
#'
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
    for(i in 1:length(x)) {
      rownames(x[[i]]) <- paste(names(x)[i], rownames(x[[i]]), sep=".")
      res <- rbind(res, x[[i]])
    }

    return(res)
}
# CHANGELOG
# 2016-04-14: update rownames
