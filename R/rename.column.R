#' Rename column(s) within a table.
#' 
#' @param x a matrix-like object
#' @param old.name a vector of column names. Can be longer than one.
#' @param new.name a vector of replacement names. must be the same length as
#'   \code{old.name}
#' @return the same x, but with column(s) renamed
#' @author Mark Cowley, 2009-06-15
#' @export
rename.column <- function(x, old.name, new.name) {
	stopifnot( length(old.name) == length(new.name) )
	stopifnot( all(old.name %in% colnames(x)) )
	
	for(i in 1:length(old.name)) {
		idx <- match(old.name[i], colnames(x))
		if( length(idx) == 0 )
			stop("column name not found.\n")
		else if( length(idx) > 1 )
			warning("Multiple columns match -- This will rename them all.\n")
		colnames(x)[idx] <- new.name[i]
	}
	
	x
}
