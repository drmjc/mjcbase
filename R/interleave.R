#' interleave the columns from multiple tables
#' 
#' interleave the columns from multiple tables. The tables
#' must be the same dimensions.
#' For tables A, B, C, the result will be
#' a1,b1,c1,a2,b2,c2,...,aN,bN,cN, where table means a 2D matrix-like
#' object
#'
#' @param \dots at least two matrix-like objects, all with the same dimensions
#' 
#' @return a \code{data.frame} with all the columns interleaved
#' 
#' @author Mark Cowley, 2005-02-22
#' @export
interleave.columns <- function(...) {
    args <- list(...)
    for(i in 2:length(args)) {
        stopifnot( nrow(args[[1]]) == nrow(args[[i]]) )
        stopifnot( ncol(args[[1]]) == ncol(args[[i]]) )
    }

    res <- matrix(NA, nrow=nrow(args[[1]]), ncol=length(args) * ncol(args[[1]]))

    colnames <- rep("", ncol(res))
    i <- 1
    for(col in 1:ncol(args[[1]])) {
        for(table in 1:length(args)) {
            res[,i] <- args[[table]][, col]
            if(!is.null(colnames(args[[table]])))
                colnames[i] <- colnames(args[[table]])[col]
            i <- i + 1
        }
    }

    rownames(res) <- rownames(args[[1]])
    if(!is.null(colnames(args[[1]])))
        colnames(res) <- colnames

    return( res )
}
