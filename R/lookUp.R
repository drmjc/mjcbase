#' generic key lookUp
#' Generic function to look up keys in a variety of data objects.
#' 
#' It works on vector or list data, where the names of each element are the
#' key.
#' It works on vectors with no names, in which case the indices of the keys are
#' returned
#' It works on tabular data, where the key is identified in \code{key.column} and
#' the columns
#' returned are specified by \code{data.columns}.
#' 
#' @param keys a vector of search keys (ie words)
#' @param data a vector, \code{matrix} or \code{data.frame}
#' @param key.column the key column if data is a \code{matrix} or \code{data.frame}. default is to use
#'   the \dQuote{row.names}
#' @param data.columns which columns to search in if \code{data} is tabular? default=\code{NULL},
#'   which searches all columns
#' @param mode find \dQuote{all} matches, or the \dQuote{first} match (faster)?
#' @return the values corresponding to the keys
#' @author Mark Cowley, 2009-07-09
#' @export
lookUp <- function(keys, data, key.column="row.names", data.columns=NULL, mode=c("all", "first")) {
	mode <-  mode[1]
	if( ! mode %in% c("all", "first") )
		stop("mode must be one of 'all' or 'first'")
	
	if( is.matrix.like(data) ) {
		# check the key.column and data.column fields.
		if( length(key.column) > 1 ) {
			warning("Only the first key column will be used.\n")
			key.column <- key.column[1]
		}
		if( key.column == "row.names" )
			data.keys <- rownames(data)
		else {
			if( is.character(key.column) ) {
				key.column <- match(key.column, colnames(data))
				if( is.na(key.column) ) {
					stop("Key not found in the colnames(data)")
				}
			}
			data.keys <- data[, key.column]
		}
		if( is.null(data.columns) ) {
			data.columns <- 1:ncol(data)
		}
		else if( is.character(data.columns) ) {
			data.columns <- match(data.columns, colnames(data))
			if( any(is.na(data.columns)) ) {
				stop("Not all data.columns found in colnames(data)")
			}
		}

		# do we find all rows that match, or the first?
		if( mode == "first")
			idx <- match(keys, data.keys)
		else if( mode == "all" )
			idx <- which(data.keys %in% keys)

		res <- data[idx, data.columns]

		# group the rows together that were indexed by the same key.
		if( mode == "all" ) {
			if (key.column != "row.names" && key.column %in% data.columns ) {
				o <- order(res[, match(key.column, data.columns)], 1:nrow(res))
				res <- res[o, ]
			}
		}
		res
	}
	else if( is.environment(data) ) {
		res <- mget(keys, data, ifnotfound=NA)
		if( length(keys) == 1 && length(res[[1]]) == 1 ) {
			res <- res[[1]]
		}
		if( all(sapply(res, length) == 1) )
			res <- unlist(res)
		names(res) <- keys
		res
	}
	else if( is.list(data) && has.names(data) ) {
		if( length(keys) == 1 && mode == "first" ) {
			res <- data[[keys]]
			names(res) <- keys
		}
		else {
			if( mode == "first" ) {
				idx <- match(keys, names(data))
			}
			else {
				idx <- which(names(data) %in% keys)
			}
			res <- ifelse(length(idx) == 1, data[[idx]], data[idx])

			if( is.list(res) && all(sapply(res, length) == 1) )
				res <- unlist(res)
			names(res) <- keys
		}
		res
	}
	else if( is.vector(data) && has.names(data)  ) {
		res <- data[keys]
		names(res) <- keys
		res
	}
	else if( is.vector(data) && !has.names(data)  ) {
		hash <- vector2hashtable(data)
		lookUp(keys, hash)
	}
	else {
		stop("Unsupported data type.")
	}
}



#' does the object have names?
#' @param x an object
#' @return \code{TRUE} if \code{x} has names, \code{FALSE} otherwise
#' @author Mark Cowley, 2009-07-09
#' @export
has.names <- function(x) {
	!is.null(names(x))
}
