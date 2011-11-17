## Function that works out the midpoints between a vector of numbers,
## eg midpoints(c(1,5,9)) will return c(3, 5)
##
## Mark Cowley, 30 June 2005
##
midpoints <- function(x) {
	res <- rep(0, length(x)-1)
	for(i in 1:(length(x)-1)) {
		res[i] <- round(mean(c(x[i], x[i+1])), 0)
	}

	return(res)
}
