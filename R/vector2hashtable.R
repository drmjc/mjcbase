#' Convert a vector to a hashtable
#' Take a vector of words, and convert the indices of each word into a
#' hashtable.
#' 
#' Useful for collapsing a many-to-one vector, where each 'key' may be found > 1 times and you
#' want some logic to choose the best row for each key.
#' Hashtable keys have to be <255 characters in length.
#' 
#' @param x a character vector, with non-unique items. sorting is not required.
#' @param warn logical: warn if the keys are >255 characters long.
#' @author Mark Cowley, 2009-07-13
#' @export
vector2hashtable <- function(x, warn=TRUE) {
	if( any(nchar(x)>255) ) {
		if( warn ) {
			warning("Some values were too long to be hashtable keys. They've been shortened to 255 characters")
		}
		x[nchar(x)>255] <- str_left(x[nchar(x)>255],255)
	}
	o <- order(x)
	x <- x[o]
	indices <- c(1:length(x))[o]
	
	res <- new.env(hash=TRUE, size=length(x))
	
	for(i in which(!is.na(x))) {
		key <- x[i]
		new.value <- indices[i]

		old.values <- tryCatch(get(key, envir=res, inherits=FALSE), error=function(e){NULL}, finally=NULL)
		values <- c(old.values, new.value)
		assign(key, values, envir=res)

	}
	res
}


#' Convert a vector of keys and values to a hashtable
#' Take a vector of keys, a vector of values, and convert into a hashtable.
#' 
#' @note Hashtable keys have to be <255 characters in length.
#' 
#' @param keys a character vector, with keys
#' @param values a vector of values. \code{length(values)==length(keys)}
#' @param make.unique logical: keep redundant key->value pairs?
#' @param sort.method the method to use to sort the values for each key
#' @author Mark Cowley, 2009-07-13
#' @export
keyval2hashtable <- function(keys, values, make.unique=TRUE, sort.method=sort) {
	# The key's can never be NA. i've also made it so that the values can't be NA but maybe that should be an argument.
	NAs <- which( is.na(keys) | is.na(values) )
	if( length(NAs) > 0 ) {
		keys <- keys[-NAs]
		values <- values[-NAs]
		if( length(keys) == 0 )
			return( NA )
	}
	
	# for each key, remember which rows in map that it is found.
	rowmap <- vector2hashtable(keys)
	
	.func <- function(x, values, make.unique, sort.method) {
		x <- values[x]
		if( !is.null(sort.method) )
			x <- sort.method( x )
		if( make.unique )
			x <- unique(x)
		x
	}
	res <- eapply(rowmap, .func, values, make.unique, sort.method)
	return( res )
	
}

#' Convert a table to a hashtable
#' Take a table with a \code{key.column}, and a \code{value.column}, and convert into a hashtable.
#' 
#' @note Hashtable keys have to be <255 characters in length.
#' 
#' @param x a \code{data.frame}
#' @param key.column the column index or name where the keys are
#' @param value.column the column index or name where the values are
#' @param make.unique logical: keep redundant key->value pairs?
#' @param sort.method the method to use to sort the values for each key
#' @author Mark Cowley, 2009-07-13
#' @export
table2hashtable <- function(x, key.column=1, value.column=2, make.unique=TRUE, sort.method=sort) {
	keyval2hashtable(x[, key.column], x[,value.column], make.unique=make.unique, sort.method=sort.method)
}
