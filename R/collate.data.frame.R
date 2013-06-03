#' Collate the columns from two data.frames
#' 
#' Collate the columns from two data.frames. ie produce a table that alternates
#' columns from the 2 tables
#' 
#' @param x two identically sized data.frames. Probably will work with matrices
#'   too.
#' @param y two identically sized data.frames. Probably will work with matrices
#'   too.
#' @return a data.frame with ncol(x)*2
#' @author Mark Cowley, 2009-12-16
#' @export
#' @examples
#' a <- iris[1:5,]; colnames(a) <- letters[1:5]
#' b <- iris[1:5,]; colnames(b) <- LETTERS[1:5]
#' collate.data.frame(a, b)
collate.data.frame <- function(x,y) {
	stopifnot(ncol(x)==ncol(y))
	stopifnot(nrow(x)==nrow(y))
	res <- cbind(x,y)
	o <- c(seq(1, ncol(res), 2), seq(2,ncol(res),2))
	o <- order(o)
	res <- res[,o]
	res
}
