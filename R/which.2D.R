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


#' @title Value matching
#' 
#' @description \code{%IN%} is a binary operator, which
#'     returns a logical \code{matrix} or \code{data.frame}, indicating if there
#'  is a match or not for its left operand in its second. Note \code{base}
#' provides \code{%in%} but this only returns a vector of logicals, even if the
#' left operand is 2D.
#'
#' @inheritParams match
#' @return a data.frame of logical values
#' @author Mark Cowley, 2012-07-27
#' @export
#' @rdname 2Dmatch
#' @seealso \code{\link{which.2D}}
#' 
#' @examples
#' if (require(datasets)) {
#' 
#' # iris is a data.frame
#' head(iris)
#' #   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#' # 1          5.1         3.5          1.4         0.2  setosa
#' # 2          4.9         3.0          1.4         0.2  setosa
#' # 3          4.7         3.2          1.3         0.2  setosa
#' # 4          4.6         3.1          1.5         0.2  setosa
#' # 5          5.0         3.6          1.4         0.2  setosa
#' # 6          5.4         3.9          1.7         0.4  setosa
#' 
#' # %in% doesn't work as expected:
#' iris %in% "setosa"
#' # [1] FALSE FALSE FALSE FALSE FALSE
#'
#' # %IN% works
#' head(iris %IN% "setosa")
#' #   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#' # 1        FALSE       FALSE        FALSE       FALSE    TRUE
#' # 2        FALSE       FALSE        FALSE       FALSE    TRUE
#' # 3        FALSE       FALSE        FALSE       FALSE    TRUE
#' # 4        FALSE       FALSE        FALSE       FALSE    TRUE
#' # 5        FALSE       FALSE        FALSE       FALSE    TRUE
#' # 6        FALSE       FALSE        FALSE       FALSE    TRUE
#' 
#' # sum on a data.frame of logical's doesn't work straight out of the box:
#' tryCatch(sum(iris %IN% "setosa"), error = function(e) x <- 1, finally=print("ERROR"))
#' sum(as.matrix(iris %IN% "setosa"))
#' # [1] 50
#' 
#' head(which.2D(as.matrix(iris %IN% "setosa")))
#' #      x y
#' # [1,] 1 5
#' # [2,] 2 5
#' # [3,] 3 5
#' # [4,] 4 5
#' # [5,] 5 5
#' # [6,] 6 5
#' }
"%IN%" <- function(x, table) {
    res <- data.frame(matrix(FALSE, nrow(x), ncol(x)))
    dimnames(res) <- dimnames(x)
    res <- lapply(x, function(x) x %in% table)
    res <- as.data.frame(res)
    res
}
