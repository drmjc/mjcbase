## Function to count the number of na's in a vector.
##
## Mark Cowley, 31 Oct 2004
##
count.na <- function(x) {
    count <- 0
    if(is.matrix(x) || is.data.frame(x)) {
        count <- sum(apply(x, 1, count.na))
    }
    else {
        for(i in 1:length(x)) {
            if(is.na(x[i]))
            count <- count + 1
        }
    }
    return(count)
}

## Function to count the number of null's in a vector.
##
## Mark Cowley, 31 Oct 2004
##
count.null <- function(x) {
    count <- 0
    if(is.matrix(x) || is.data.frame(x)) {
        count <- sum(apply(x, 1, count.null))
    }
    else {
        for(i in 1:length(x)) {
            if(is.null(x[i]))
            count <- count + 1
        }
    }
    return(count)
}
