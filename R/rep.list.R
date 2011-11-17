## repeat the object in what N times to make an N element list
##
## Mark Cowley, 7 April 2006
##
rep.list <- function(what, N) {

    res <- list()
    res[1:N] <- what

    return( res )
}
