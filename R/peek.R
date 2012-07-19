#' A function to show first 5 rows and col of data
#' 
#' @param table a \code{matrix} or \code{data.frame}
#' @param x the number of rows to show
#' @param y the number of columns to show
#' @return none. prints the specified parts of the table
#' @author Mark Cowley
#' @export
#' @seealso \code{\link{head}} \code{\link{tail}}
peek <- function(table, x=5, y=5) {
    if(is.matrix(table) | is.data.frame(table)) {
	    x <- min(x, nrow(table))
	    y <- min(y, ncol(table))
        # if(x > nrow(table)) x <- nrow(table)
        # if(y > ncol(table)) y <- ncol(table)
        table[c(1:x), c(1:y)]
    }
    else if(is.list(table)) {
        cat(paste("peeking from 1st elem in list: ($", names(table)[1], ")\n", sep=""))
        peek(table[[1]], x, y)
    }
    else if(length(table) >= 1) {
        if(x > length(table)) x <- length(table)
        table[c(1:x)]
    }
}
