#' which.2D
#' 
#' @description \code{which.2D}: Which cells are \code{TRUE} in a matrix-like object
#' 
#' @param x a \code{matrix} or \code{data.frame}
#' @param add.names logical: if \code{TRUE}, add \code{x.name} and \code{y.name}
#'	columns to the result
#' 
#' @return \code{which.2D}: a 2-column matrix of row and column indices,
#'	indicating the coordinates of the \code{TRUE} values in \code{x}
#' 
#' @author Mark Cowley, 12/4/07
#' 
#' @export
#' @rdname which.2D
#' 
#' @examples
#' x <- matrix(rnorm(25), 5, 5)
#' which.2D(x>1)
#' #	  x y
#' # [1,] 1 4
#' # [2,] 4 3
which.2D <- function(x, add.names=FALSE) {
	N <- sum(x, na.rm=TRUE)
	res <- matrix(0,N,2)
	colnames(res) <- c("x", "y")

	idx <- 1
	for(row in which( rowSums(x, na.rm=TRUE) > 0 )) {
		cols <- which(x[row,])
		for(col in cols) {
			res[idx,] <- c(row,col)
			idx <- idx + 1
		}
	}
	if( add.names && !is.null(rownames(x)) && !is.null(rownames(x)) ) {
		res <- as.data.frame(res, stringsAsFactors=FALSE)
		res$x.name <- rownames(x)[res[,1]]
		res$y.name <- colnames(x)[res[,2]]
	} 
	
	res
}

#' @description \code{which.min.2D}: Which is the minimum cell in a matrix-like object
#'
#' @param return.ties logical: if there are ties in the top N, then return
#'	all values in the top N, or just N?
#' 
#' @return \code{which.min.2D}: a vector of length 2 indicating the x and y indices respectively
#'
#' @author Mark Cowley, 2 Nov 2005
#' 
#' @export
#' @rdname which.2D
which.min.2D <- function(x, return.ties=FALSE) {
	res <- which.minN.2D(x, 1, return.ties=return.ties)
	if( !return.ties ) {
		res <- res[1,]
		names(res) <- NULL
	}
	return( res )
}

#' @description \code{which.minN.2D}: Which are the minimum N cells in a matrix-like object
#'
#' @return \code{which.minN.2D}: a \code{matrix} with 2 columns, col 1 for the rows and col 2 for the cols
#' @author Mark Cowley, 2 Nov 2005
#'
#' @export
#' @rdname which.2D
which.minN.2D <- function(x, N, return.ties=FALSE) {
	min.idx <- which.minN(x, N, return.ties=return.ties)
	row <- min.idx %% nrow(x)
	row[row==0] <- nrow(x)

	col <- ceiling(min.idx / nrow(x))

	res <- cbind(row, col)
	colnames(res) <- c("x", "y")
	rownames(res) <- rownames(x)[res[,1]]
	res
}


#' @description \code{which.max.2D}: Which is the maximum cell in a matrix-like object
#' 
#' @return \code{which.max.2D}: a vector of length 2 indicating the x and y indices respectively
#' @author Mark Cowley, 2 Nov 2005
#' @export
#' @rdname which.2D
which.max.2D <- function(x, return.ties=FALSE) {
	res <- which.maxN.2D(x, 1, return.ties=return.ties)
	if( !return.ties ) {
		res <- res[1,]
		names(res) <- NULL
	}
	return( res )
}

#' @description \code{which.maxN.2D}: Which are the maximum N cells in a matrix-like object
#' 
#' @param N top 'N' values to return
#' 
#' @return \code{which.maxN.2D}: a matrix with 2 columns, col 1 for the rows and col 2 for the cols
#' 
#' @author Mark Cowley, 2 Nov 2005
#' @export
#' @rdname which.2D
which.maxN.2D <- function(x, N, return.ties=FALSE) {
	max.idx <- which.maxN(x, N, return.ties=return.ties)
	row <- max.idx %% nrow(x)
	row[row==0] <- nrow(x)

	col <- ceiling(max.idx / nrow(x))

	res <- cbind(row, col)
	colnames(res) <- c("x", "y")
	rownames(res) <- rownames(x)[res[,1]]
	res
}

