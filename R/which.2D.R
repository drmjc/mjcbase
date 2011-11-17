# Function to work out which cells are TRUE in a matrix-like object
#
# Mark Cowley, 12/4/07
which.2D <- function(x, add.names=F) {
    N <- sum(x, na.rm=TRUE)
    res <- matrix(0,N,2)
    colnames(res) <- c("x", "y")

    idx <- 1
    for(row in which( rowSums(x, na.rm=T) > 0 )) {
        cols <- which(x[row,])
        for(col in cols) {
            res[idx,] <- c(row,col)
            idx <- idx + 1
        }
    }
    if( add.names && !is.null(rownames(x)) && !is.null(rownames(x)) ) {
        res <- as.data.frame(res, stringsAsFactors=FALSE)
        res$x.name <- rownames(x)[res[,1]]
        res$y.name <- colnames(x)[res[,2]]
    } 
    
    res
}

## Which is the minimum cell in a matrix-like object
##
## Parameters:
##     x: a matrix or data.frame
##
## Value:
##     a vector of length 2 indicating the x and y indices respectively
##
## Mark Cowley, 2 Nov 2005
##
which.min.2D <- function(x, return.ties=F) {
    res <- which.minN.2D(x, 1, return.ties=return.ties)
    if( !return.ties ) {
        res <- res[1,]
        names(res) <- NULL
    }
    return( res )
}

## Which are the minimum N cells in a matrix-like object
##
## Parameters:
##     x: a matrix or data.frame
##
## Value:
##     a matrix with 2 columns, col 1 for the rows and col 2 for the cols
##
## Mark Cowley, 2 Nov 2005
##
which.minN.2D <- function(x, N, return.ties=F) {
    min.idx <- which.minN(x, N, return.ties=return.ties)
    row <- min.idx %% nrow(x)
    row[row==0] <- nrow(x)

    col <- ceiling(min.idx / nrow(x))

    res <- cbind(row, col)
    colnames(res) <- c("x", "y")
    rownames(res) <- rownames(x)[res[,1]]
    res
}




## Which is the maximum cell in a matrix-like object
##
## Parameters:
##     x: a matrix or data.frame
##
## Value:
##     a vector of length 2 indicating the x and y indices respectively
##
## Mark Cowley, 2 Nov 2005
##
which.max.2D <- function(x, return.ties=F) {
    res <- which.maxN.2D(x, 1, return.ties=return.ties)
    if( !return.ties ) {
        res <- res[1,]
        names(res) <- NULL
    }
    return( res )
}


## Which are the maximum N cells in a matrix-like object
##
## Parameters:
##     x: a matrix or data.frame
##
## Value:
##     a matrix with 2 columns, col 1 for the rows and col 2 for the cols
##
## Mark Cowley, 2 Nov 2005
##
which.maxN.2D <- function(x, N, return.ties=F) {
    max.idx <- which.maxN(x, N, return.ties=return.ties)
    row <- max.idx %% nrow(x)
    row[row==0] <- nrow(x)

    col <- ceiling(max.idx / nrow(x))

    res <- cbind(row, col)
    colnames(res) <- c("x", "y")
    rownames(res) <- rownames(x)[res[,1]]
    res
}
