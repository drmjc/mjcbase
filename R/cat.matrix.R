#' Print a matrix to stdout.
#' 
#' @param x a \code{matrix}
#' @param row.names logical: print the row names?
#' @param col.names logical: print the col names?
#' @param pad logical: pad the values in each column?
#' @param sep the separator between columns.
#' 
#' @return none. it prints a matrix to console.
#' 
#' @author Mark Cowley
#' @export
cat.matrix <- function (x, row.names=TRUE, col.names=TRUE, pad=TRUE, sep=" ") {
	if(row.names && !is.null(rownames(x))) x <- rownames2col(x, 1, " ")
	if(col.names && !is.null(colnames(x))) x <- colnames2row(x, 1, " ")
	if( pad ) {
		for(i in 1:ncol(x)) {
			x[,i] <- as.character(x[,i])
			w <- max(nchar(x[,i]))
			cmd <- paste("%",w,"s", sep="")
			x[,i] <- sprintf(cmd, x[,i])
		}
	}
	for (i in 1:nrow(x)) {
		for (j in 1:ncol(x)) {
			cat(x[i, j], sep, sep = "")
		}
		cat("\n")
	}
}

