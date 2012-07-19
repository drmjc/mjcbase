#' move columns/rows to col/rownames & back again
#' 
#' @description \code{column2rownames, col2rownames}: Move a column of \code{x} to the \code{rownames} and remove it from \code{x}
#' 
#' @param x a \code{data.frame}
#' @param column the column index that the newly moved rownames will have - (ie
#'   column=1 puts the rownames into the first column). If column=NULL or
#'   column > ncol(x), the rownames will be moved to the first, and last
#'   column, respectively.
#' @param sep the value to use to make the rownames unique (if necessary)
#' 
#' @return \code{column2rownames, col2rownames}: \code{x} with 1 less column and with \code{rownames} set
#' 
#' @author Mark Cowley, 31 May 2006
#' @export
#' @rdname column2rownames
column2rownames <- function(x, column=1, sep=".") {
    rownames(x) <- make.unique(as.character(x[, column]), sep=sep)
    x <- x[,-column]

    return( x )
}

#' @export
#' @rdname column2rownames
col2rownames <- column2rownames

#' @description \code{row2colnames}: Move a row of x to the colnames and remove it from x
#' 
#' @param row the row index that the newly moved colnames will have - (ie
#'   row=1 puts the colnames into the first row). If row=NULL or
#'   row > nrow(x), the colnames will be moved to the first, and last
#'   row, respectively.
#' 
#' @return \code{row2colnames}: \code{x} with 1 less row and with \code{colnames} set
#' @author Mark Cowley, 31 May 2006
#' @export
#' @rdname column2rownames
row2colnames <- function(x, row=1, sep=".") {
    colnames(x) <- make.unique( as.character(unlist(x[row, ])), sep="." )
    x <- x[-row, ]

    return( x )
}


#' @description \code{rownames2col,rownames2column}: Move the rownames of x into a column
#' 
#' @param colname what name should the new column have? only gets set if there
#'   was a colname to begin with.
#' 
#' @return \code{rownames2col,rownames2column}: a \code{data.frame} with \code{ncol(x) + 1} and the rownames in the extra column.
#'   The colclasses will be the same as they were in \code{x}, and the rownames will
#'   have the class of \dQuote{character}.
#' 
#' @author Mark Cowley, 31 May 2006
#' @export
#' @rdname column2rownames
rownames2col <- function(x, column=1, colname="") {
	rn <- NULL
	if( is.null(rownames(x)) ) {
		rn <- rep("", nrow(x))
	}
	else {
		rn <- rownames(x)
	}

    res <- data.frame(rn, x, check.names=FALSE)
	colclasses(res) <- c("character", colclasses(x))
	
	if( is.null(column) ) column <- 1
	else if( is.character(column) ) column <- match(column, colnames(x))
	else if( is.numeric(column) && column < 1) column <- 1
	else if( is.numeric(column) && column > ncol(x)) column <- ncol(x) + 1
	
	if( !is.null(colnames(x)) ) {
		colnames(res) <- c(colname, colnames(x))
	}
	idx <- c(column, setdiff(1:ncol(res), column))
	res <- res[, idx]

    return( res )
}
# CHANGELOG
# 2011-11-01: bug fix in return(x) -> return(res)

#' @export
#' @rdname column2rownames
rownames2column <- rownames2col

#' @description \code{colnames2row}: Move the colnames of x into a row.
#' 
#' @param rowname what name should the new row have? only gets set if there
#'   was a rowname to begin with.
#' 
#' @return \code{colnames2row}: a \code{data.frame} with \code{nrow(x) + 1} and the colnames in the extra row.
#'   The rowclasses will be the same as they were in \code{x}, and the colnames will
#'   have the class of \dQuote{character}.
#' 
#' @author Mark Cowley, 31 May 2006
#' @export
#' @rdname column2rownames
colnames2row <- function(x, row=1, rowname) {
	t(rownames2col(t(x),row,rowname))
}
