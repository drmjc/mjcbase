## Function to linearly scale a vector of numbers (x) to be between min and max.
##
## Mark Cowley, 31 Oct 2005
##
scale2 <- function(x, min, max) {
    range.x <- diff(range(x, na.rm=T)) ## want this to become 1.0
    if( range.x == 0 )
        return( rep(mean(c(min,max)), length(x)) )

    x2 <- (x-min(x, na.rm=T))# * 1/range.x ## + min(x)

    new.range <- max - min
    res <- x2 * new.range + min
    res <- res / range.x
    res
}
