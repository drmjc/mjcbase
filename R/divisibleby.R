## Function to see integers can be divided into x to yield integers
##
## Mark Cowley, 21 Feb 2006
##
divisibleby <- function(x, limit=2000) {
    res <- numeric(0)

    for(i in 2:limit) {
        if(x %% i == 0)
            res <- c(res, i)
    }

    return(res)
}
