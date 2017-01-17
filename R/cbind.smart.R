#' A smarter cbind
#' 
#' \code{cbind} only combines matrix-like objects when they contain identical rownames.
#' This function allows any number of matrix-like objects to be cbound, even if they
#' contain very few (or even no) rownames in common. The result will have 1 row per unique
#' rowname across all the input matrix-like-objects, and will have \code{NA}'s inserted
#' where appropriate.
#' The first argument will be in the top left of the result, followed by all columns of the
#' 2nd argument, and any new rows added to the bottom, until the final obect is added in
#' the bottom right hand corner. Thus the result grows diagonally.
#' 
#' @param \dots at least two matrix-like objects to be merged.
#' @param sort.col Column to sort on. \dQuote{NULL} is ok.
#' @return a \code{data.frame} with all input objects cbound together, and with 1 row per unique
#' rowname across all the input matrix-like-objects, with \code{NA}'s inserted
#' where appropriate.
#' @author Mark Cowley, 2010-03-10
#' @seealso \code{\link{rbind.smart}}, \code{\link{cbind}}
#' @keywords manip
#' @examples
#' a <- data.frame(matrix(rnorm(25), 5, 5))
#' dimnames(a) <- list(letters[1:5], LETTERS[1:5])
#' b <- data.frame(matrix(rnorm(25), 5, 5))
#' dimnames(b) <- list(letters[3:7], LETTERS[3:7])
#' c <- data.frame(matrix(rnorm(25), 5, 5))
#' dimnames(c) <- list(letters[11:15], LETTERS[11:15])
#' cbind.smart(a, b, c)
#' @export
#' @importFrom methods as
cbind.smart <- function(..., sort.col=NULL) {
	args <- list(...)
	
	res <- args[[1]]
	for(i in 2:length(args)) {
		res <- .cbind.smart.2way(res, args[[i]], sort.col=sort.col)
	}
	res
}

.cbind.smart.2way <- function(x, y, sort.col=NULL) {
	ROWNAMES <- union(rownames(x), rownames(y))
	# keep rownames in the order that they were in "x".
	tmp.order <- match(rownames(x), ROWNAMES)
	ROWNAMES <- ROWNAMES[c(tmp.order, setdiff(1:length(ROWNAMES), tmp.order))]

	# x <- x[ROWNAMES, ]; rownames(x) <- ROWNAMES
	# y <- y[ROWNAMES, ]; rownames(y) <- ROWNAMES
	# res <- cbind(x,y)
	res <- as.data.frame(matrix(NA, length(ROWNAMES), ncol(x)+ncol(y)))
	rownames(res) <- ROWNAMES
	colnames(res) <- make.unique(c(colnames(x), colnames(y)))
	colnames(x) <- colnames(res)[1:ncol(x)]
	colnames(y) <- colnames(res)[(ncol(x)+1):ncol(res)]
	res[rownames(x),colnames(x)] <- x
	res[rownames(y),colnames(y)] <- y
	
	if( !is.null(sort.col) )
		res <- res[order(res[,sort.col]),]

	if( is.matrix(x) && is.matrix(y) )
		res <- as.matrix(res)

	return( res )
}