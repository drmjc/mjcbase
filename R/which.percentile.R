#' For a numeric vector x, return which percentile each data point is in
#' @param x numeric vector
#' @return return the percentile of each data point is in, relative to the whole vector.
#' @author Mark Cowley, 2008-12-22
#' @seealso \code{\link{which.quartile}}
#' @export
which.percentile <- function(x) {
	perc <- percentile(x, na.rm=TRUE)
	perc[1] <- -Inf
	perc[length(perc)] <- Inf
	# cat(perc, "\n")
	
	res <- rep(0,length(x))
	for(i in 1:length(x)) {
		res[i] <- names(perc)[max(which(perc < x[i]))]
	}
	res
}


#' For a numeric vector x, return which quartile each data point is in
#' @param x numeric vector
#' @return return the quartile of each data point is in, relative to the whole vector.
#' @author Mark Cowley, 2011-09-28
#' @seealso \code{\link{which.percentile}}
#' @export
which.quartile <- function(x) {
	quar <- quartile(x, na.rm=TRUE)
	quar[1] <- -Inf
	quar[length(quar)] <- Inf
	# cat(quar, "\n")
	
	res <- rep(0,length(x))
	for(i in 1:length(x)) {
		res[i] <- names(quar)[max(which(quar < x[i]))]
	}
	res
}
