#' sort numerically
#'
#' sort a vector of words numerically, as opposed to alphanumerically.
#' If your words contain a number, eg string1, string2, string3, ..., string10
#' then the standard sort would return string10, string1, string2, ...
#' and converting to numeric would fail.
#' 
#' @param x character vector where each word may contain a number
#' @param decreasing logical: Should the sort be increasing or decreasing?  Not
#'  available for partial sorting.
#' @return a sorted charcter vector
#' @author Mark Cowley, 2005-05-03
#' @export
sortnum <- function(x, decreasing=FALSE) {
    x2 <- as.numeric(gsub("[^0-9]", "", x))
    if(all(!is.na(x2)))
        x <- x[order(x2, decreasing=decreasing)]
    else {
        isnum <- which(!is.na(x2))
        if( length(isnum) > 0 )
            x <- c(sortnum(x[isnum], decreasing=FALSE), sort(x[-isnum], decreasing=FALSE))
        else
            x <- sort(x, decreasing=FALSE)

        if( decreasing )
            x <- rev( x )
    }

    return( x )
}

#' sort files in a directory numerically
#'
#' If your files contain a simple pattern, like:\cr
#' tmp1.out, tmp2.out, \dots, tmp10.out, \dots\cr
#' and you want to process them in this order:\cr
#' 1,2,\dots,10,\dots, then use \code{dir.sort}.
#' \code{dir()} does an alphanumeric
#' sort, so you would get tmp10.out, tmp1.out, ...
#' 
#' @param path the path to a directory. default=current working directory
#' @param pattern an optional regular expression.  Only file names which match
#'        the regular expression will be returned.
#' @param \dots arguments passed to \code{\link{dir}}
#' 
#' @return A character vector containing the names of the files in the
#'   specified directories, or \code{""} if there were no files.  If a path
#'   does not exist or is not a directory or is unreadable it is
#'   skipped, with a warning.
#'
#' @author Mark Cowley, 2005-05-03
#' @export
dir.sort <- function(path=".", pattern=NULL, ...) {
    files <- dir(path, pattern, ...)

    files <- sortnum(files)

    return( files )
}
