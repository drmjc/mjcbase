## Function to compare 2 vectors. It finds any elements that aren't equal, and returns the values
## in a matrix
##
## Parameters:
##     x,y: 2 vectors or numerics or characters
##     delta: if x and y are numerics, then the difference in the values is also calculated
##
## Value:
##     NULL if all(x==y); or
##     a data.frame with 1 row per mismatch and 2 or 3 columns. cols 1 and 2 are the different values in x and y,
##     and 3rd col is the delta or difference betweeen the x and y values for each mismatch.
##
## Mark Cowley, 14 June 2005
##
diff.2way <- function(x, y, which=TRUE, delta=TRUE) {
    if( !is.numeric(x) )
        delta <- FALSE

    res <- NULL
    if(!all(x==y)) {
        tmp <- which(x != y)
        ncol <- 2 + which + delta
         res <- matrix(NA, length(tmp), ncol)
        if(which) {
            if(delta)
                colnames(res) <- c("x", "y", "which", "delta")
            else
                colnames(res) <- c("x", "y", "which")
        }
        else {
            if(delta)
                colnames(res) <- c("x", "y", "delta")
            else
                colnames(res) <- c("x", "y")
        }

        for(i in 1:length(tmp)) {
            res[i, 1] <- x[tmp[i]]
            res[i, 2] <- y[tmp[i]]
        }

        if(which)
            res[,3] <- tmp

        if(delta) ## col 4 if which==T, col3 if which==F
            res[,3+which] <- diff(apply(res[,1:2], 1, range))

        if(!is.null(names(x)) & all(names(x)==names(y)))
            rownames(res) <- names(x)[tmp]

        res <- data.frame(res)
    }
    return(res)
}
