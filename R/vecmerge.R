#' Merge a vector and a data.frame
#' 
#' Merge a \code{vector} and a \code{data.frame}.
#' This will essentially subset \code{y}
#' to the values of \code{y$"by.y"} to the values specified in \code{x}. If values in \code{x}
#' aren't found in \code{y}, then \code{NA} fields will be filled in.
#' 
#' @param x a vector of \sQuote{search keys}
#' @param y a \code{data.frame} of values
#' @param by.y the column name in y to match the \sQuote{search keys} in \code{x}. To match
#'   against the \code{rownames(y)}, set this to \code{0}, or \dQuote{\code{row.names}}
#' @param all.x logical: return all values in \code{x}, even if they're not in \code{y}?
#' @param all.y logical: return all rows in \code{y}, even if they're not in \code{x}?
#' @param sort logical: Should the results be sorted on the \code{by.y} column?
#' @param unique.x logical: if \code{TRUE}, then keep at most one match for each value of \code{x}; 
#'    if \code{FALSE}, then allow aech value in \code{x} to match multiple rows in \code{y}
#' @return a data.frame with same columns as \code{y}, but with rows specified by 
#'    the search keys in \code{x}, and the value of \code{unique.x}.
#' @author Mark Cowley, 18 April 2006
#' @export
vecmerge <- function(x, y, by.y="", all.x=TRUE, all.y=FALSE, sort=FALSE, 
					 unique.x=TRUE) {

	if( by.y == "row.names" || by.y == 0 ) {
		by.y <- "row.names"
	}
	else if( is.numeric(by.y) ) {
		by.y <- colnames(y)[by.y]
	}
	else if (! by.y %in% colnames(y)) {
		stop(sprintf("unsupported value for by: '%s'\n", as.character(by.y)))
	}

	if( sort ) {
		x <- sort(x)
	}
	
	#
	# convert x into a matrix, and add an artificial column
	# that remembers the order that x was in. This will be
	# removed after the merge.
	#
	tmp <- data.frame(x=x, y=1:length(x), stringsAsFactors=FALSE)
	colnames(tmp) <- c(by.y, "XXXXorder")
	rownames(tmp) <- make.names(tmp[,1])

	res <- merge(x=tmp, y=y, by.x=1, by.y=by.y, all.x=all.x, all.y=all.y, sort=FALSE)

	# re-sort the res so that it's the same order as the original x.
	res <- res[order(res$XXXXorder),]

	#
	# keep the col orders as per columns in y
	#
	res <- res[, colnames(y)]

	#
	# keep the first row per unique "x"
	#
	if( unique.x && by.y != "row.names" )
		res <- res[match(x, res[,match(by.y, colnames(y))]), ]

	rownames(res) <- 1:nrow(res)

	return(res)
}
# CHANGELOG
# 2011-08-05: bug fix in R 2.12 to avoid factors sneaking in in the tmp <- data.frame... line

test.vecmerge <- function() {
	x <- c("a", "b", "c")                                               
	y <- data.frame(colA=c("a", "a", "b", "b", "c", "d", "e"), colB=1:7)
	print(vecmerge(x,y,"colA", unique.x=FALSE))
	print(vecmerge(x,y,"colA", unique.x=TRUE))
}
