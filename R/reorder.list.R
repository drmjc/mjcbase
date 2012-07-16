#' Reorder the elements in a list
#'
#' @param x a \code{list}
#' @param order vector of indices 
#' @param \dots unused
#' @return a \code{list}
#' @author Mark Cowley, 2011-11-17
#' 
#' @export
#' @importFrom stats reorder
#' @aliases reorder reorder.list
#' 
#' @usage reorder(x, order, ...)
#' 
#' @examples
#' l <- list(a=1,b=2,c=3)
#' reorder.list(l, c(2,1,3))
#' reorder(l, c(2,1,3))
reorder.list <- function(x, order, ...) {
	res <- list()
	for(i in 1:length(x)) {
		res[[i]] <- x[[ order[i] ]]
	}
	names(res) <- names(x)[order]
	return(res)
}
