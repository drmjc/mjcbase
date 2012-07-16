#' count the number of NA's
#'
#' @param x a 1D or 2D object
#' 
#' @return a numeric(1) indicating how many \code{NA}'s were present.
#' 
#' @author Mark Cowley, 31 Oct 2004
#' @export
count.na <- function(x) {
	sum(is.na(x))
	# count <- 0
	# if(is.matrix(x) || is.data.frame(x)) {
	#     count <- sum(apply(x, 1, count.na))
	# }
	# else {
	#     count <- sum(is.na(x))
	# }
	# return(count)
}
# CHANGELOG
# 2012-07-07: my this is old, inefficient code!

#' count the number of NULL's
#'
#' @param x a 1D or 2D object
#' 
#' @return a numeric(1) indicating how many \code{NA}'s were present.
#' 
#' @author Mark Cowley, 31 Oct 2004
#' @export
count.null <- function(x) {
	sum(is.null(x))
	# count <- 0
	# if(is.matrix(x) || is.data.frame(x)) {
	#     count <- sum(apply(x, 1, count.null))
	# }
	# else {
	#     count <- sum(is.null(count))
	#     for(i in 1:length(x)) {
	#         if(is.null(x[i]))
	#         count <- count + 1
	#     }
	# }
	# return(count)
}
# CHANGELOG
# 2012-07-07: my this is old, inefficient code!