## extracts the nth col (or row) from each matrix in a list
##
## Parameters:
##     x: list containing matrices
##     n: integer of the column (or row) to be extracted
##     row: logical indicating if it is the rows thats to be extracted
##
## Value:
##     matrix containing the nth row or col from each matrix in the list.
##
## Eva Chan 28/4/05
##
get.mat.in.ls <- function(x, n = 1, row = F) {

    if ( !row ) {
        res <- matrix(unlist(lapply(x, "[", 1:nrow(x[[1]]), n)), ncol = length(x))
    }
    else {
        res <- matrix(unlist(lapply(x, "[", n, 1:ncol(x[[1]]))), nrow = length(x), byrow = T)
    }

    return( res )
}


get.row.fromlist <- function(x, n) {
    return( get.mat.in.ls(x, n, row=T) )
}


get.col.fromlist <- function(x, n) {
    return( get.mat.in.ls(x, n, row=F) )
}
