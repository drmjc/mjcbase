#' Is the given path a file or directory?
#' 
#' @param path the path to a file or directory
#' @return \code{is.dir}: \dQuote{TRUE} if the path is a directory, \dQuote{FALSE} otherwise
#' @author Mark Cowley
#' @export
#' @rdname is.dir
is.dir <- function(path) {
    return( !is.na(file.info(path)$isdir) & file.info(path)$isdir )
}

#' @return \code{is.file}: \dQuote{TRUE} if the path is a file, \dQuote{FALSE} otherwise
#' @export
#' @rdname is.dir
is.file <- function(path) {
    return( !is.na(file.info(path)$isdir) & !file.info(path)$isdir )
}
