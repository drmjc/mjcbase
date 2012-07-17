# Reorder a column, or block of columns from within a data.frame.
# The colnames must not be null, and must be unique
#
# Parameters:
#	x: a data.frame
#	cols2move: The column(s) that you want to move. Can be a vector of length >= 1 of either column names, or indices. If more than one are specified, and they are not contiguous, they will be properly extracted from the data.frame, but brought together into a contiguous block, then inserted into result before the specified column
#	at: which column do you want the new columns to be inserted BEFORE. if <= 1, then move to fron, if > ncol(x), or at=NULL, then move the columns to end.
#
# Mark Cowley, 2009-01-20
#


#' Reorder a column, or block of columns from within a data.frame.
#' 
#' The colnames must not be null, and must be unique
#' 
#' @param x a data.frame
#' @param cols2move The column(s) that you want to move. Can be a vector of
#'   length >= 1 of either column names, or indices. If more than one are
#'   specified, and they are not contiguous, they will be properly extracted
#'   from the data.frame, but brought together into a contiguous block, then
#'   inserted into result before the specified column
#' @param at which column do you want the new columns to be inserted BEFORE. if
#'   <= 1, then move to fron, if > ncol(x), or at=NULL, then move the columns
#'   to end.
#' @author Mark Cowley, 2009-01-20
#' @export
move.column <- function(x, cols2move, at) {
	# make the cols to pull out be characters, but the insertion index be numeric.
	if( is.numeric(cols2move) )
		cols2move <- colnames(x)[cols2move]
	if( !all(cols2move %in% colnames(x)) )
		stop("These columns were not found & thus could not be moved: ", setdiff(cols2move, colnames(x)), "\n")

	if( is.character(at) )
		at <- match(at, colnames(x))

	if( at <= 1 ) {
		cols <- setdiff(colnames(x), cols2move)
		x <- x[,c(cols2move, cols)]
	}
	else if( is.null(at) || at > ncol(x) ) {
		cols <- setdiff(colnames(x), cols2move)
		x <- x[,c(cols, cols2move)]
	}
	else {
		# first shuffle the block to the end.
		at <- colnames(x)[at]
		x <- x[, c(setdiff(colnames(x), cols2move), cols2move)]
		at <- which(colnames(x) == at)
		N <- length(cols2move)
		# now last N columns are the block that needs to be moved.
		
		cols <- c(1:(at-1),
				  (ncol(x)-N+1):ncol(x),
				  at:(ncol(x)-N))
		x <- x[,cols]
	}
	x
}
