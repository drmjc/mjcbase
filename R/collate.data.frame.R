# Collate the columns from two data.frames. ie produce a table that alternates columns from the 2 tables
#
# Parameters:
#	x, y: two identically sized data.frames. Probably will work with matrices too.
#
# Value:
#	a data.frame with ncol(x)*2
#
# Mark Cowley, 2009-12-16
#


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
collate.data.frame <- function(x,y) {
	stopifnot(ncol(x)==ncol(y))
	stopifnot(nrow(x)==nrow(y))
	res <- cbind(x,y)
	o <- c(seq(1, ncol(res), 2), seq(2,ncol(res),2))
	o <- order(o)
	res <- res[,o]
	res
}
