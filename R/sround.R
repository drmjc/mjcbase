## Smart round function.
##
##
sround <- function(x, digits) {
	if(!is.null(dim(x)))
		return(apply(x, 2, sround, digits))
	else
		for(i in 1:length(x)) {
			if(x[i] <= 10^-digits)
				x[i] <- x[i]
			else
				x[i] <- round(x[i], digits)
		}
	x
}
